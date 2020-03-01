{-# LANGUAGE OverloadedStrings #-}
module Chargen
    ( app
    , runApp
    )
where

import           Network.Wai                    ( Application )
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Header
import           Network.Wai.Middleware.Cors
import qualified Web.Scotty                    as S
import           Character.Attributes
import           Control.Monad.IO.Class
import           Queries

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy Nothing
                                [methodPost, methodGet, methodOptions]
                                [hContentType, hAuthorization]
                                Nothing
                                Nothing
                                False
                                False
                                False

-- | Define routes
app' :: S.ScottyM ()
app' = do
    S.middleware $ cors (const $ Just corsPolicy)
    S.get "/character" $ do
        char <- liftIO $ nRandomCharacters 1 defaultOptions randomAttributes3D6
        S.json char
    S.post "/character" $ do
        queryOpt <- S.jsonData
        case validateQuery queryOpt of
            Right q ->
                liftIO (nRandomCharacters (count q) q randomAttributes3D6)
                    >>= S.json
            Left _ ->
                S.json ("Invalid query" :: String) >> S.status badRequest400

-- | This is exported for use in automated tests
app :: IO Application
app = S.scottyApp app'

-- | Start the application
runApp :: IO ()
runApp = S.scotty 8080 app'
