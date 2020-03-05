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
import           Character.Race
import           Character.Class
import           Queries
import           Data.Set                      as Set

main :: IO ()
main = hspec spec

allClasses = fromList [Assassin ..]

allRaces = fromList [Dwarf ..]

postBody = encode $ QueryOptions { count           = 10
                                 , minLevel        = 2
                                 , maxLevel        = 5
                                 , selectedClasses = allClasses
                                 , selectedRaces   = allRaces
                                 , attributeGen    = Method3D6
                                 }
minGTmax = encode $ QueryOptions { count           = 10
                                 , minLevel        = 3
                                 , maxLevel        = 1
                                 , selectedClasses = allClasses
                                 , selectedRaces   = allRaces
                                 , attributeGen    = Method3D6
                                 }

impossibleRaceClassCombination = encode $ QueryOptions
    { count           = 5
    , minLevel        = 1
    , maxLevel        = 5
    , selectedClasses = Set.singleton Druid
    , selectedRaces   = Set.fromList [Elf, Dwarf]
    , attributeGen    = Method4D6BestOf3
    }

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

    describe "POST /character : impossible race-class-combination"
        $                   it "responds with 400"
        $                   post "/character" impossibleRaceClassCombination
        `shouldRespondWith` 400
