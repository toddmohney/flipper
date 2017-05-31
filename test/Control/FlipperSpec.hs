module Control.FlipperSpec (main, spec) where

import qualified Data.Map.Strict     as M
import           Test.Hspec

import           Control.Flipper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "whenEnabled" $ do
        context "the feature is enabled" $
            it "evaluates the given monad" $ do
              let featureState = mkFeatures $ M.insert "ADD_ANOTHER_FEATURE" True mempty
              store <- execFlipper featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
              M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just True

        context "the feature is disabled" $
            it "does not evaluate the given monad" $ do
                let featureState = mkFeatures $ M.insert "ADD_ANOTHER_FEATURE" False mempty
                store <- execFlipper featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Nothing

    describe "enable" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipper featureState (enable "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a existing feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" False mempty
                result = evalFlipper featureState (enable "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

    describe "disable" $ do
        it "disables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipper featureState (disable "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` False

        it "disables a existing feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" True mempty
                result = evalFlipper featureState (disable "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "toggle" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipper featureState (toggle "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a disabled feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" False mempty
                result = evalFlipper featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

        it "disables a enabled feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" True mempty
                result = evalFlipper featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "enabled" $ do
        it "returns True when the feature is enabled" $
            let
                featureState = mkFeatures $ M.insert "ENABLED_FEATURE" True mempty
                result = evalFlipper featureState (enabled "ENABLED_FEATURE")
            in
                result `shouldReturn` True

        it "returns False when the feature is not enabled" $
            let
                featureState = mkFeatures $ M.insert "DISABLED_FEATURE" False mempty
                result = evalFlipper featureState (enabled "DISABLED_FEATURE")
            in
                result `shouldReturn` False

        it "returns False when the feature key is not found" $
            let
                featureState = mempty :: Features
                result = evalFlipper featureState (enabled "NON_EXISTANT_FEATURE")
            in
                result `shouldReturn` False
