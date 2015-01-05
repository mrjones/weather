{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum)
import Happstack.Server (bindPort, nullConf, ok, port, toResponse, Response, ServerPartT, simpleHTTPWithSocket)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve $ HubConfig 5999

data HubConfig =
  HubConfig { hubPort :: Int }

serve :: HubConfig -> IO ()
serve config = do
  let httpConf = nullConf { port = hubPort config }
  socket <- bindPort nullConf { port = hubPort config }
  putStrLn $ "Serving on port: " ++ (show (hubPort config))
  simpleHTTPWithSocket socket httpConf $ allPages config


allPages :: HubConfig -> ServerPartT IO Response
allPages config =
  msum [ helloPage ]

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
