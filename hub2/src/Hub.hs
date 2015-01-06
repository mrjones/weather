{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Digest.CRC32 (crc32)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)
import Happstack.Server (badRequest, bindPort, dir, look, nullConf, ok, port, toResponse, Response, ServerPartT, simpleHTTPWithSocket)
import Happstack.Server.RqData (checkRq, getDataFn, RqData)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

main :: IO ()
main = serve $ HubConfig 5999

data HubConfig =
  HubConfig { hubPort :: Int }

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
  simpleHTTPWithSocket socket httpConf $ allPages


allPages :: ServerPartT IO Response
allPages =
  msum [ dir "report" $ reportPage
       , helloPage ]

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

reportPage :: ServerPartT IO Response
reportPage = do
  ts <- getDataFn (dataPointParams `checkRq` parseDataPoint)
  case ts of
    (Left e) -> badRequest $ toResponse $ reportPageHtml $ unlines e
    (Right (s)) -> ok $ toResponse $ reportPageHtml (show s)

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
