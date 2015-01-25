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
import Data.Map (fromList, Map)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Happstack.Server (asContentType, badRequest, bindPort, dir, look, nullConf, ok, port, toResponse, Response, serveFile, ServerPartT, simpleHTTPWithSocket)
import Happstack.Server.RqData (checkRq, getDataFn, RqData)
import Database.MySQL.Simple (connectUser, connectPassword, connectDatabase, connectHost, defaultConnectInfo, execute)
import qualified Database.MySQL.Simple as MySQL (connect, Connection, query)
import Database.MySQL.Simple.QueryResults (QueryResults, convertError, convertResults)
import Database.MySQL.Simple.Result (convert)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), getOpt, OptDescr(..), usageInfo)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(LineBuffering), stdout)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "-----"
  argv <- getArgs
  flags <- parseFlags argv
  putStrLn $ "Flags: " ++ show flags
  serve $ configFromFlags flags

data Flag = DBUsernameFlag String
          | DBPasswordFlag String
          | DBHostnameFlag String
          | PortFlag Int
          | MakeDatabaseFlag
          | StaticDirFlag String deriving (Show)

flagDefs :: [OptDescr Flag]
flagDefs =
  [ Option ['u'] ["dbuser"] (ReqArg dbUserFlag "USER") "MySQL username"
  , Option ['w'] ["dbpass"] (ReqArg dbPassFlag "PASS") "MySQL password"
  , Option ['h'] ["dbhost"] (ReqArg dbHostFlag "HOST") "MySQL hostname"
  , Option ['p'] ["port"] (ReqArg portFlag "PORT") "HTTP port"
  , Option ['m'] ["mkdb"] (NoArg MakeDatabaseFlag) "Create the DB"
  , Option ['s'] ["staticdir"] (ReqArg staticDirFlag "DIR") "Static dir"
  ]

dbUserFlag, dbPassFlag, dbHostFlag, portFlag, staticDirFlag :: String -> Flag
dbUserFlag = DBUsernameFlag
dbPassFlag = DBPasswordFlag
dbHostFlag = DBHostnameFlag
portFlag ms = PortFlag $ fromMaybe 5999 $ readMaybe ms
staticDirFlag = StaticDirFlag

parseFlags :: [String] -> IO [Flag]
parseFlags argv = 
  case getOpt Permute flagDefs argv of
    (f,_,[]  ) -> return f
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flagDefs))
  where header = "Usage: hub [OPTION...]"

configFromFlags :: [Flag] -> HubConfig
configFromFlags fs =
  foldr (\f c -> case f of
            DBUsernameFlag u -> c { hubDbUsername = u }
            DBPasswordFlag p -> c { hubDbPassword = p }
            DBHostnameFlag h -> c { hubDbHostname = h }
            PortFlag p -> c { hubPort = p }
            MakeDatabaseFlag -> c { hubCreateDatabase = True }
            StaticDirFlag s -> c { hubStaticDir = s }
        ) defaultConfig fs

defaultConfig :: HubConfig
defaultConfig = HubConfig 5999 "weather" "weather" "localhost" False "static"

type SeriesID = Int

data HubConfig = HubConfig { hubPort :: Int
                           , hubDbUsername :: String
                           , hubDbPassword :: String
                           , hubDbHostname :: String
                           , hubCreateDatabase :: Bool
                           , hubStaticDir :: String
                           }

data Point = Point { dpValue :: Int
                   , dpSeriesId :: SeriesID
                   , dpTimestamp :: UTCTime
                   , dpReporterId :: Int
                   } deriving (Show)

data Query = Query { querySeriesId :: SeriesID
                   , queryDurationSec :: Int
                   } deriving (Show)

data QueryResponse =
  QueryResponse { responsePoints :: [Point]
                , responseConnectTimeUsec :: Int
                , responseQueryTimeUsec :: Int
                , responseTotalTimeUsec :: Int
                , responseReporterNames :: Map String String
                }

serve :: HubConfig -> IO ()
serve config = do
  if hubCreateDatabase config
     then mkDatabase config
     else putStrLn "Not creating DB"
  let httpConf = nullConf { port = hubPort config }
  socket <- bindPort nullConf { port = hubPort config }
  putStrLn $ "Serving on port: " ++ (show (hubPort config))
  simpleHTTPWithSocket socket httpConf $ allPages config

staticFilename :: HubConfig -> String -> String
staticFilename config relativeName =
  (hubStaticDir config) ++ "/" ++ relativeName

allPages :: HubConfig -> ServerPartT IO Response
allPages config = do
  requestStartTime <- liftIO $ getCurrentTime
  msum [ dir "js" $ dir "app.js" $ serveFile (asContentType "text/javascript") (staticFilename config "/app.js")
       , dir "css" $ dir "hub.css" $ serveFile (asContentType "text/css") (staticFilename config "hub.css")
       , dir "report" $ reportPage config
       , dir "query" $ queryPage config requestStartTime
       , dashboardPage
       ]

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

mkDatabase :: HubConfig -> IO ()
mkDatabase config = do
  conn <- dbConnect config
  _ <- execute conn "CREATE TABLE history (\
                    \ value BIGINT,\
                    \ series_id BIGINT,\
                    \ timestamp DATETIME,\
                    \ reporter_id INT,\
                    \ PRIMARY KEY(series_id, timestamp, reporter_id))" ()
  return ()

storePoint :: MySQL.Connection -> Point -> IO Bool
storePoint conn dp = do
  fmap ((==) 1) $ execute conn "INSERT INTO history (value, series_id, timestamp, reporter_id) VALUES (?, ?, ?, ?)" (dpValue dp, dpSeriesId dp, dpTimestamp dp, dpReporterId dp)

instance QueryResults Point where
  convertResults [f_val, f_sid, f_ts, f_rid] [v_val, v_sid, v_ts, v_rid] =
    Point (convert f_val v_val) (convert f_sid v_sid) (convert f_ts v_ts) (convert f_rid v_rid)
  convertResults fs vs = convertError fs vs 4

queryPoints :: MySQL.Connection -> Query -> IO [Point]
queryPoints conn q = do
  MySQL.query conn "SELECT value, series_id, timestamp, reporter_id FROM history WHERE series_id = (?) AND timestamp > DATE_SUB(NOW(), INTERVAL (?) SECOND)" (querySeriesId q, queryDurationSec q)

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

instance ToJSON QueryResponse where
  toJSON r = JSON.object [ "points" .= (responsePoints r)
                         , "connTimeUsec" .= (responseConnectTimeUsec r)
                         , "queryTimeUsec" .= (responseQueryTimeUsec r)
                         , "totalTimeUsec" .= (responseTotalTimeUsec r)
                         , "reporterNames" .= (responseReporterNames r)
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
  value <- look "v"
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

-- TODO(mrjones): make this configurable
reporterNames :: Map String String
reporterNames = fromList [("1", "Living Room"), ("3", "Bedroom")]

queryPage :: HubConfig -> UTCTime -> ServerPartT IO Response
queryPage conf startTime = do
  equery <- getDataFn $ queryParams `checkRq` parseQuery
  case equery of
    Left e -> badRequest $ toResponse $ reportPageHtml $ unlines e
    Right query -> do
      timeA <- liftIO $ getCurrentTime
      conn <- liftIO $ dbConnect conf
      timeB <- liftIO $ getCurrentTime
      points <- liftIO $ queryPoints conn query
      timeC <- liftIO $ getCurrentTime
      ok $ toResponse $ (LC8.unpack . JSON.encode) $
        QueryResponse points (toUsec (diffUTCTime timeB timeA)) (toUsec (diffUTCTime timeC timeB)) (toUsec (diffUTCTime timeC startTime)) reporterNames

toUsec :: NominalDiffTime -> Int
toUsec dt = floor (1000 * 1000 * (realToFrac dt :: Double))


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
      importJs "https://cdnjs.cloudflare.com/ajax/libs/dygraph/1.1.0/dygraph-combined-dev.js"
      importJs "/js/app.js"
      H.link ! A.href "http://fonts.googleapis.com/css?family=Roboto" ! A.rel "stylesheet" ! A.type_ "text/css"
      H.link ! A.href "/css/hub.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.body ! A.onload "init()" $ do
      H.div ! A.id "temps_div" $ ""
      H.div ! A.id "humid_div" $ ""
      H.div ! A.id "debug" $ ""
