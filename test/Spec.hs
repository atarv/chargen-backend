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
                                                )
import           Chargen                        ( app )

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
    describe "GET /hello" $ do
        it "responds with 'Hello, world!'"
            $                   get "/hello"
            `shouldRespondWith` "Hello, world!"

        it "responds with 200 / 'Hello, world!'"
            $                   get "/hello"
            `shouldRespondWith` "Hello, world!" { matchStatus = 200 }

        it "has 'Content-Type: text/plain; charset=utf-8'"
            $                   get "/hello"
            `shouldRespondWith` 200
                                    { matchHeaders =
                                        [ "Content-Type"
                                              <:> "text/plain; charset=utf-8"
                                        ]
                                    }
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
    describe "GET /character/10"
        $                   it "responds with 200 and JSON content"
        $                   get "/character/10"
        `shouldRespondWith` 200
                                { matchHeaders =
                                    [ "Content-Type"
                                          <:> "application/json; charset=utf-8"
                                    ]
                                }




--     describe "GET /some-json" $ it "responds with some JSON" $ do
--             get "/some-json" `shouldRespondWith` expectedJsonResponse

-- expectedJsonResponse =
--     let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
--     in  ResponseMatcher status
--                         [hContentType <:> "application/json; charset=utf-8"]
--                         body
