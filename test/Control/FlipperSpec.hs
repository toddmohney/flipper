module Control.FlipperSpec (main, spec) where

import qualified Data.Map.Strict                 as M
import           Test.Hspec

import           Control.Flipper
import           Control.Flipper.Adapters.Memory

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "whenEnabled" $ do
        context "the feature is enabled" $
            it "evaluates the given monad" $ do
              let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" (Feature True) mempty
              store <- execFlipperT featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
              M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (Feature True)

        context "the feature is disabled" $
            it "does not evaluate the given monad" $ do
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" (Feature False) mempty
                store <- execFlipperT featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Nothing

    describe "enable" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (enable "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a existing feature key" $
            let
                featureState = Features $ M.insert "FEATURE" (Feature False) mempty
                result = evalFlipperT featureState (enable "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

    describe "disable" $ do
        it "disables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (disable "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` False

        it "disables a existing feature key" $
            let
                featureState = Features $ M.insert "FEATURE" (Feature True) mempty
                result = evalFlipperT featureState (disable "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "toggle" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (toggle "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a disabled feature key" $
            let
                featureState = Features $ M.insert "FEATURE" (Feature False) mempty
                result = evalFlipperT featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

        it "disables a enabled feature key" $
            let
                featureState = Features $ M.insert "FEATURE" (Feature True) mempty
                result = evalFlipperT featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "enabled" $ do
        it "returns True when the feature is enabled" $
            let
                featureState = Features $ M.insert "ENABLED_FEATURE" (Feature True) mempty
                result = evalFlipperT featureState (enabled "ENABLED_FEATURE")
            in
                result `shouldReturn` True

        it "returns False when the feature is not enabled" $
            let
                featureState = Features $ M.insert "DISABLED_FEATURE" (Feature False) mempty
                result = evalFlipperT featureState (enabled "DISABLED_FEATURE")
            in
                result `shouldReturn` False

        it "returns False when the feature key is not found" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (enabled "NON_EXISTANT_FEATURE")
            in
                result `shouldReturn` False
