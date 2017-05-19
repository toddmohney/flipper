module FlipperSpec (main, spec) where

import           Control.Monad.State (evalState)
import qualified Data.Map.Strict     as M
import           Test.Hspec

import           Flipper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "enable" $ do
        it "enables a new feature key" $
            let
                featureState = mempty
                result = evalState (enable "NEW_FEATURE" >> enabled "NEW_FEATURE") featureState
            in
                result `shouldBe` True

        it "enables a existing feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" False mempty
                result = evalState (enable "FEATURE" >> enabled "FEATURE") featureState
            in
                result `shouldBe` True

    describe "disable" $ do
        it "disables a new feature key" $
            let
                featureState = mempty
                result = evalState (disable "NEW_FEATURE" >> enabled "NEW_FEATURE") featureState
            in
                result `shouldBe` False

        it "disables a existing feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" True mempty
                result = evalState (disable "FEATURE" >> enabled "FEATURE") featureState
            in
                result `shouldBe` False

    describe "flipFeature" $ do
        it "enables a new feature key" $
            let
                featureState = mempty
                result = evalState (flipFeature "NEW_FEATURE" >> enabled "NEW_FEATURE") featureState
            in
                result `shouldBe` True

        it "enables a disabled feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" False mempty
                result = evalState (flipFeature "FEATURE" >> enabled "FEATURE") featureState
            in
                result `shouldBe` True

        it "disables a enabled feature key" $
            let
                featureState = mkFeatures $ M.insert "FEATURE" True mempty
                result = evalState (flipFeature "FEATURE" >> enabled "FEATURE") featureState
            in
                result `shouldBe` False

    describe "enabled" $ do
        it "returns True when the feature is enabled" $
            let
                featureState = mkFeatures $ M.insert "ENABLED_FEATURE" True mempty
                result = evalState (enabled "ENABLED_FEATURE") featureState
            in
                result `shouldBe` True

        it "returns False when the feature is not enabled" $
            let
                featureState = mkFeatures $ M.insert "DISABLED_FEATURE" False mempty
                result = evalState (enabled "DISABLED_FEATURE") featureState
            in
                result `shouldBe` False

        it "returns False when the feature key is not found" $
            let
                featureState = mempty
                result = evalState (enabled "NON_EXISTANT_FEATURE") featureState
            in
                result `shouldBe` False
