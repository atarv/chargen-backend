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
import           System.Environment             ( withArgs )

main :: IO ()
main = hspec spec

allClasses = fromList [Assassin ..]
allRaces = fromList [Dwarf ..]

baseQuery = QueryOptions { count           = 10
                         , minLevel        = 1
                         , maxLevel        = 5
                         , selectedClasses = allClasses
                         , selectedRaces   = allRaces
                         , attributeGen    = Method3D6
                         }

postBody = encode baseQuery
minGTmax = encode $ baseQuery { minLevel = 30, maxLevel = 1 }

impossibleRaceClassCombination = encode $ baseQuery
    { selectedClasses = Set.singleton Druid
    , selectedRaces   = Set.fromList [Elf, Dwarf]
    , attributeGen    = Method4D6BestOf3
    }

noSelectedClasses = encode $ baseQuery { selectedClasses = mempty }
noSelectedRaces = encode $ baseQuery { selectedRaces = mempty }

spec :: Spec
spec = with (app "./assets/chargen.db") $ do
    describe "GET /character"
        $                   it "responds with 200 and JSON content"
        $                   get "/character"
        `shouldRespondWith` 200
                                { matchHeaders =
                                    [ "Content-Type"
                                          <:> "application/json; charset=utf-8"
                                    ]
                                }

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

    describe "POST /character : no selected classes"
        $                   it "responds with 400"
        $                   post "/character" noSelectedClasses
        `shouldRespondWith` 400

    describe "POST /character : no selected races"
        $                   it "responds with 400"
        $                   post "/character" noSelectedRaces
        `shouldRespondWith` 400

    describe "POST /character : too high count"
        $ it "responds with 400"
        $ post "/character" (encode $ baseQuery { count = 1001 })
        `shouldRespondWith` 400
