{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Chargen (app, runApp) where

import Network.Wai (Application, pathInfo, Request)
import qualified Web.Scotty as S
import Data.Aeson
import Data.Aeson.Types

app' :: S.ScottyM ()
app' = do
    S.get "/hello" $ do
        S.text "Hello, world!"
    S.post "/post" $ do
        b <- S.body
        S.setHeader "Content-Type" "text/plain"
        S.raw b


app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'