{-# LANGUAGE OverloadedStrings #-}
module Chargen
    ( app
    , runApp
    )
where

import           Network.Wai                    ( Application )
import           Network.HTTP.Types.Status
import qualified Web.Scotty                    as S
import           Character.Attributes
import           Control.Monad.IO.Class
import           Queries

-- | Define routes
app' :: S.ScottyM ()
app' = do
    S.get "/character" $ do
        char <- liftIO $ nRandomCharacters 1 defaultOptions randomAttributes3D6
        S.json char
    S.post "/character" $ do
        queryOpt <- S.jsonData
        case validateQuery queryOpt of
            Right q ->
                liftIO (nRandomCharacters (count q) q randomAttributes3D6)
                    >>= S.json
            Left msg -> S.text msg >> S.status badRequest400

-- | This is exported for use in automated tests
app :: IO Application
app = S.scottyApp app'

-- | Start the application
runApp :: IO ()
runApp = S.scotty 8080 app'
