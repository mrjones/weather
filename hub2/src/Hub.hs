{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (optional)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LC8 (unpack)
import Data.Aeson (toJSON, ToJSON, (.=))
import qualified Data.Aeson as JSON (encode, object)
import Data.Digest.CRC32 (crc32)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Happstack.Server (asContentType, badRequest, bindPort, dir, look, nullConf, ok, port, toResponse, Response, serveFile, ServerPartT, simpleHTTPWithSocket)
import Happstack.Server.RqData (checkRq, getDataFn, RqData)
import Database.MySQL.Simple (connectUser, connectPassword, connectDatabase, connectHost, defaultConnectInfo, execute)
import qualified Database.MySQL.Simple as MySQL (connect, Connection, query)
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults)
import Database.MySQL.Simple.Result (convert)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), getOpt, OptDescr(..), usageInfo)
import System.Environment (getArgs)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

main :: IO ()
main = do
  argv <- getArgs
  flags <- parseFlags argv
  putStrLn $ show flags
  serve $ configFromFlags flags

data Flag = DBUsername String
          | DBPassword String
          | DBHostname String
          | Port Int deriving (Show)

flags :: [OptDescr Flag]
flags =
  [ Option ['u'] ["dbuser"] (ReqArg dbUserFlag "USER") "MySQL user"
  , Option ['w'] ["dbpass"] (ReqArg dbPassFlag "PASS") "MySQL password"
  , Option ['h'] ["dbhost"] (ReqArg dbHostFlag "HOST") "MySQL host"
  , Option ['p'] ["port"] (ReqArg portFlag "PORT") "Port"
  ]

dbUserFlag, dbPassFlag, dbHostFlag, portFlag :: String -> Flag
dbUserFlag = DBUsername
dbPassFlag = DBPassword
dbHostFlag = DBHostname
portFlag ms = Port $ fromMaybe 5999 $ readMaybe ms

parseFlags :: [String] -> IO [Flag]
parseFlags argv = 
  case getOpt Permute flags argv of
    (f,_,[]  ) -> return f
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flags))
  where header = "Usage: hub [OPTION...]"

configFromFlags :: [Flag] -> HubConfig
configFromFlags fs =
  foldr (\f c -> case f of
            DBUsername u -> c { hubDbUsername = u }
            DBPassword p -> c { hubDbPassword = p }
            DBHostname h -> c { hubDbHostname = h }
            Port p -> c { hubPort = p }
        ) defaultConfig fs

defaultConfig :: HubConfig
defaultConfig = HubConfig 5999 "weather" "weather" "localhost"

type SeriesID = Int

data HubConfig = HubConfig { hubPort :: Int
                           , hubDbUsername :: String
                           , hubDbPassword :: String
                           , hubDbHostname :: String
                           }

data Point = Point { dpValue :: Int
                   , dpSeriesId :: SeriesID
                   , dpTimestamp :: UTCTime
                   , dpReporterId :: Int
                   } deriving (Show)

data Query = Query { querySeriesId :: SeriesID
                   , queryDurationSec :: Int
                   } deriving (Show)

serve :: HubConfig -> IO ()
serve config = do
  let httpConf = nullConf { port = hubPort config }
  socket <- bindPort nullConf { port = hubPort config }
  putStrLn $ "Serving on port: " ++ (show (hubPort config))
  simpleHTTPWithSocket socket httpConf $ allPages config

allPages :: HubConfig -> ServerPartT IO Response
allPages config =
  msum [ dir "js" $ serveFile (asContentType "text/javascript") "static/app.js"
       , dir "report" $ reportPage config
       , dir "query" $ queryPage config
       , dashboardPage
       ]

dbPages :: MySQL.Connection -> ServerPartT IO Response
dbPages conn =
  msum [ ]

--
-- Database
--

dbConnect :: HubConfig -> IO MySQL.Connection
dbConnect conf = do
  putStrLn "DBCONNECT"
  MySQL.connect defaultConnectInfo
    { connectUser = hubDbUsername conf
    , connectPassword = hubDbPassword conf
    , connectDatabase = "weather"
    , connectHost = hubDbHostname conf
    }



storePoint :: MySQL.Connection -> Point -> IO Bool
storePoint conn dp = do
  fmap ((==) 1) $ execute conn "INSERT INTO history (value, series_id, timestamp, reporter_id) VALUES (?, ?, ?, ?)" (dpValue dp, dpSeriesId dp, dpTimestamp dp, dpReporterId dp)

instance QueryResults Point where
  convertResults [f_val, f_sid, f_ts, f_rid] [v_val, v_sid, v_ts, v_rid] =
    Point (convert f_val v_val) (convert f_sid v_sid) (convert f_ts v_ts) (convert f_rid v_rid)

queryPoints :: MySQL.Connection -> Query -> IO [Point]
queryPoints conn q =
  MySQL.query conn "SELECT value, series_id, timestamp, reporter_id FROM history WHERE series_id = (?) AND timestamp > DATE_SUB(CURDATE(), INTERVAL (?) SECOND)" (querySeriesId q, queryDurationSec q)

--
-- JSON
--

toUnix :: UTCTime -> Int
toUnix = round . utcTimeToPOSIXSeconds

instance ToJSON Point where
  toJSON p = JSON.object [ "ts" .= (toUnix (dpTimestamp p))
                         , "val" .= (dpValue p)
                         , "rid" .= (dpReporterId p)
                         ]

--
-- Misc/common
--

verboseReadEither :: Read a => String -> Either String a
verboseReadEither s = case readMaybe s of
  Just v -> Right v
  Nothing -> Left $ "Could not parse: " ++ s


seriesNameToId :: String -> SeriesID
seriesNameToId = fromIntegral . crc32 . C8.pack

--
-- SimpleReport
-- /simplereport?t_sec=1234567890&v=42&tsname=es.mrjon.metric&rid=2222
--

dataPointParams :: RqData (String, String, String, String)
dataPointParams = do
  seriesName <- look "tsname"
  timestamp <- look "t_sec"
  value <- look "value"
  rid <- look "rid"
  return (seriesName, timestamp, value, rid)

parsePoint :: (String, String, String, String) -> Either String Point
parsePoint (seriesName, timestampS, valueS, ridS) = do
  seriesId <- return $ seriesNameToId seriesName
  unixTime <- verboseReadEither timestampS :: Either String Int
  utcTime <- return $ posixSecondsToUTCTime $ fromIntegral unixTime
  value <- verboseReadEither valueS
  rid <- verboseReadEither ridS
  return $ Point value seriesId utcTime rid

reportPage :: HubConfig -> ServerPartT IO Response
reportPage conf = do
  mdp <- getDataFn (dataPointParams `checkRq` parsePoint)
  case mdp of
    (Left e) -> badRequest $ toResponse $ reportPageHtml $ unlines e
    (Right dp) -> do
      conn <- liftIO $ dbConnect conf
      success <- liftIO $ storePoint conn dp
      ok $ toResponse $ reportPageHtml ((show dp) ++ (show success))

reportPageHtml :: String -> H.Html
reportPageHtml msg =
  H.html $ do
    H.head $ do
      H.title "Data reported"
    H.body $ do
      H.div ! A.id "body" $ H.toHtml ("Message: " ++ msg)
--
-- Query
-- /query?tsname=es.mrjon.foo&secs=3600
-- 

queryParams :: RqData (String, Maybe String)
queryParams = do
  seriesName <- look "tsname"
  duration <- optional $ look "secs"
  return (seriesName, duration)

parseQuery :: (String, Maybe String) -> Either String Query
parseQuery (seriesName, mduration) = do
  duration <- verboseReadEither (fromMaybe "86400" mduration)
  Right $ Query (seriesNameToId seriesName) duration

queryPage :: HubConfig -> ServerPartT IO Response
queryPage conf = do
  equery <- getDataFn $ queryParams `checkRq` parseQuery
  case equery of
    Left e -> badRequest $ toResponse $ reportPageHtml $ unlines e
    Right query -> do
      conn <- liftIO $ dbConnect conf
      points <- liftIO $ queryPoints conn query
      ok $ toResponse $ (LC8.unpack . JSON.encode) points


--
-- Dashboard
--

dashboardPage :: ServerPartT IO Response
dashboardPage = ok $ toResponse $ dashboardHtml

importJs :: H.AttributeValue -> H.Html
importJs url = H.script ! A.type_ "text/javascript" ! A.src url $ ""

dashboardHtml :: H.Html
dashboardHtml =
  H.html $ do
    H.head $ do
      H.title "Fortress Weather"
      importJs "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
      importJs "https://www.google.com/jsapi"
      importJs "/js/app.js"
    H.body ! A.onload "init()" $ do
      H.div ! A.id "temps_div" $ ""
      H.div ! A.id "humid_div" $ ""
