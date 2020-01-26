{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Chargen
    ( app
    , runApp
    )
where

import           Network.Wai                    ( Application
                                                , pathInfo
                                                , Request
                                                )
import qualified Web.Scotty                    as S
import           Data.Aeson
import           Data.Aeson.Types
import           Character.Attributes
import           Control.Monad
import           Control.Monad.IO.Class

app' :: S.ScottyM ()
app' = do
    S.get "/hello" $ do
        S.text "Hello, world!"
    S.post "/post" $ do
        b <- S.body
        S.setHeader "Content-Type" "text/plain"
        S.raw b
    S.get "/attributes/:count" $ do
        c     <- S.param "count" :: S.ActionM Int
        attrs <- liftIO $ replicateM c randomAttributes3D6
        S.json attrs

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'
