{-# LANGUAGE OverloadedStrings #-}
module Chargen
    ( app
    , runApp
    )
where

import           Network.Wai                    ( Application )
import qualified Web.Scotty                    as S
import           Character.Attributes
import           Control.Monad
import           Control.Monad.IO.Class
import           Queries

-- | Define routes
app' :: S.ScottyM ()
app' = do
    S.get "/hello" $ S.text "Hello, world!"
    S.post "/post" $ do
        b <- S.body
        S.setHeader "Content-Type" "text/plain"
        S.raw b
    S.get "/attributes/:count" $ do
        c     <- S.param "count" :: S.ActionM Int
        attrs <- liftIO $ replicateM c randomAttributes3D6
        S.json attrs
    S.get "/alignment" $ do
        a <- liftIO $ randomAlignment
        S.json a
    S.get "/alignment/:count" $ do
        c          <- S.param "count" :: S.ActionM Int
        alignments <- liftIO $ replicateM c randomAlignment
        S.json alignments
    S.get "/character" $ do
        char <- liftIO $ nRandomCharacters 1 randomAttributes3D6
        S.json char
    S.get "/character/:count" $ do
        c     <- S.param "count" :: S.ActionM Int
        chars <- liftIO $ nRandomCharacters c randomAttributes3D6
        S.json chars

-- | This is exported for use in automated tests
app :: IO Application
app = S.scottyApp app'

-- | Start the application
runApp :: IO ()
runApp = S.scotty 8080 app'
