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
import           Control.Monad.IO.Class
import           Queries
import           System.Environment             ( getArgs )

app' :: String -> S.ScottyM ()
app' db = do
    S.middleware $ cors (const $ Just corsPolicy)
    -- Define routes
    S.get "/character"
        $   liftIO (nRandomCharacters db 1 defaultOptions)
        >>= S.json
    S.post "/character" $ do
        queryOpt <- S.jsonData
        case validateQuery queryOpt of
            Right q@QueryOptions { count = c } ->
                liftIO (nRandomCharacters db c q) >>= S.json
            Left msg -> S.json ("Invalid query: " ++ msg :: String)
                >> S.status badRequest400

-- | This is exported for use in automated tests
app :: String -> IO Application
app = S.scottyApp . app'

-- | Start the application
runApp :: IO ()
runApp = do
    args <- getArgs
    if null args
        then error "Error: path to character database not given"
        else S.scotty 8080 $ app' (head args)

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy Nothing
                                [methodPost, methodGet, methodOptions]
                                [hContentType, hAuthorization]
                                Nothing
                                Nothing
                                False
                                False
                                False
