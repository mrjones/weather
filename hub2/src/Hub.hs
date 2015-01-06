{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Digest.CRC32 (crc32)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)
import Happstack.Server (badRequest, bindPort, dir, look, nullConf, ok, port, toResponse, Response, ServerPartT, simpleHTTPWithSocket)
import Happstack.Server.RqData (checkRq, getDataFn, RqData)
import Database.MySQL.Simple (connectUser, connectPassword, connectDatabase, connectHost, defaultConnectInfo, execute)
import qualified Database.MySQL.Simple as MySQL (connect, Connection)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

main :: IO ()
main = serve $ HubConfig 5999 "weather" "weather" "localhost"

data HubConfig =
  HubConfig { hubPort :: Int
            , hubDbUsername :: String
            , hubDbPassword :: String
            , hubDbHostname :: String
            }

data DataPoint =
  DataPoint { dpTimeseriesId :: Word32
            , dpTimestamp :: UTCTime
            , dpValue :: Int
            , dpReporterId :: Int
            } deriving (Show)

serve :: HubConfig -> IO ()
serve config = do
  let httpConf = nullConf { port = hubPort config }
  socket <- bindPort nullConf { port = hubPort config }
  putStrLn $ "Serving on port: " ++ (show (hubPort config))
  simpleHTTPWithSocket socket httpConf $ allPages config

allPages :: HubConfig -> ServerPartT IO Response
allPages config = do
  conn <- liftIO $ dbConnect (hubDbUsername config) (hubDbPassword config) (hubDbHostname config)
  msum [ dir "report" $ reportPage conn
       , helloPage ]

--
-- Database
--

dbConnect :: String -> String -> String -> IO MySQL.Connection
dbConnect username password hostname = MySQL.connect defaultConnectInfo
    { connectUser = username
    , connectPassword = password
    , connectDatabase = "weather"
    , connectHost = hostname
    }

storeDataPoint :: MySQL.Connection -> DataPoint -> IO Bool
storeDataPoint conn dp = do
  fmap ((==) 1) $ execute conn "INSERT INTO history (value, series_id, timestamp, reporter_id) VALUES (?, ?, ?, ?)" (dpValue dp, dpTimeseriesId dp, dpTimestamp dp, dpReporterId dp)

--
-- SimpleReport
-- /simplereport?t_sec=1234567890&v=42&tsname=es.mrjon.metric&rid=2222
--
verboseReadEither :: Read a => String -> Either String a
verboseReadEither s = case readMaybe s of
  Just v -> Right v
  Nothing -> Left $ "Could not parse: " ++ s

dataPointParams :: RqData (String, String, String, String)
dataPointParams = do
  tsname <- look "tsname"
  timestamp <- look "t_sec"
  value <- look "value"
  rid <- look "rid"
  return (tsname, timestamp, value, rid)

parseDataPoint :: (String, String, String, String) -> Either String DataPoint
parseDataPoint (seriesIdS, timestampS, valueS, ridS) = do
  seriesId <- return $ crc32 . C8.pack $ seriesIdS
  unixTime <- verboseReadEither timestampS :: Either String Int
  utcTime <- return $ posixSecondsToUTCTime $ fromIntegral unixTime
  value <- verboseReadEither valueS
  rid <- verboseReadEither ridS
  return $ DataPoint seriesId utcTime value rid

reportPage :: MySQL.Connection -> ServerPartT IO Response
reportPage conn = do
  mdp <- getDataFn (dataPointParams `checkRq` parseDataPoint)
  case mdp of
    (Left e) -> badRequest $ toResponse $ reportPageHtml $ unlines e
    (Right dp) -> do
      success <- liftIO $ storeDataPoint conn dp
      ok $ toResponse $ reportPageHtml ((show dp) ++ (show success))

reportPageHtml :: String -> H.Html
reportPageHtml msg =
  H.html $ do
    H.head $ do
      H.title "Data reported"
    H.body $ do
      H.div ! A.id "body" $ H.toHtml ("Message: " ++ msg)

--
--
--

helloPage :: ServerPartT IO Response
helloPage = do
  ok $ toResponse $ helloHtml


helloHtml :: H.Html
helloHtml =
  H.html $ do
    H.head $ do
      H.title "Hello, world!"
    H.body $ do
      H.div ! A.id "body" $ "Hello, world!"