{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    )
where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                , encode
                                                )
import           Chargen                        ( app )
import           Queries

main :: IO ()
main = hspec spec


postBody = encode $ QueryOptions { count = 10, minLevel = 2, maxLevel = 5 }
minGTmax = encode $ QueryOptions { count = 10, minLevel = 3, maxLevel = 1 }

spec :: Spec
spec = with app $ do
    describe "GET /character"
        $                   it "responds with 200 and JSON content"
        $                   get "/character"
        `shouldRespondWith` 200
                                { matchHeaders =
                                    [ "Content-Type"
                                          <:> "application/json; charset=utf-8"
                                    ]
                                }
        -- it "responds with a random OSRIC character" 
    describe "POST /character"
        $                   it "responds with 200 and JSON content"
        $                   post "/character" postBody
        `shouldRespondWith` 200
                                { matchHeaders =
                                    [ "Content-Type"
                                          <:> "application/json; charset=utf-8"
                                    ]
                                }
    describe "POST /character : invalid level constraints"
        $                   it "responds with 400"
        $                   post "/character" minGTmax
        `shouldRespondWith` 400





--     describe "GET /some-json" $ it "responds with some JSON" $ do
--             get "/some-json" `shouldRespondWith` expectedJsonResponse

-- expectedJsonResponse =
--     let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
--     in  ResponseMatcher status
--                         [hContentType <:> "application/json; charset=utf-8"]
--                         body
