import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "How to write a test" $ do
        it "Should be able to run tests" $ do
            ((length languages) >= 10) `shouldBe` (True)

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
        it "Should returns nothing if it can not find the word" $ do
            findWord grid "CRYSTAL" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist on the grid" $ do
            findWords grid langs `shouldBe` languages
            where langs = languages ++ ["CRYSTAL", "RUST"]
