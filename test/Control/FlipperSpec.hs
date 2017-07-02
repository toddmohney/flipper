module Control.FlipperSpec (main, spec) where

import qualified Data.ByteString.Char8           as C8
import           Data.Default
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
                let feature = def { isEnabled = True }
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" feature mempty
                store <- execFlipperT featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (def { isEnabled = True })

        context "the feature is disabled" $
            it "does not evaluate the given monad" $ do
                let feature = def { isEnabled = False }
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" feature mempty
                store <- execFlipperT featureState (whenEnabled "ADD_ANOTHER_FEATURE" (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Nothing

    describe "whenEnabledFor" $ do
        context "the feature is enabled globally" $
            it "evaluates the given monad" $ do
                let feature = Feature { enabledEntities = [], enabledPercentage = 0, isEnabled = True }
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" feature mempty
                store <- execFlipperT featureState (whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (def { isEnabled = True })

        context "the feature is enabled for the given actor" $
            it "evaluates the given monad" $ do
                let feature = Feature { enabledEntities = [actorId myActor], enabledPercentage = 0, isEnabled = False }
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" feature mempty
                store <- execFlipperT featureState (whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (def { isEnabled = True })

        context "the feature is not enabled for the given actor" $
            it "does not evaluate the given monad" $ do
                let feature = Feature { enabledEntities = [], enabledPercentage = 0, isEnabled = False }
                let featureState = Features $ M.insert "ADD_ANOTHER_FEATURE" feature mempty
                store <- execFlipperT featureState (whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (enable "ANOTHER_FEATURE"))
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
                feature = def { isEnabled = False }
                featureState = Features $ M.insert "FEATURE" feature mempty
                result = evalFlipperT featureState (enable "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

    describe "enableFor" $ do
        describe "with an existing feature" $ do
            it "enables the feature only for the given actor" $ do
                let feature = Feature { enabledEntities = [], enabledPercentage = 0, isEnabled = False }
                let featureState = Features $ M.insert "FEATURE" feature mempty
                let result1 = evalFlipperT featureState (enableFor "FEATURE" myActor >> enabledFor "FEATURE" myActor)
                let result2 = evalFlipperT featureState (enableFor "FEATURE" myActor >> enabledFor "FEATURE" myOtherActor)

                result1 `shouldReturn` True
                result2 `shouldReturn` False

        describe "with a non-existant feature" $ do
            it "enables the feature only for the given actor" $ do
                let featureState = mempty :: Features
                let result1 = evalFlipperT featureState (enableFor "FEATURE" myActor >> enabledFor "FEATURE" myActor)
                let result2 = evalFlipperT featureState (enableFor "FEATURE" myActor >> enabledFor "FEATURE" myOtherActor)

                result1 `shouldReturn` True
                result2 `shouldReturn` False

    describe "enabledFor" $ do
        it "returns True if the feature is enabled globally" $ do
            let feature = Feature { enabledEntities = [], enabledPercentage = 0, isEnabled = True }
            let featureState = Features $ M.insert "FEATURE" feature mempty
            let result = evalFlipperT featureState (enabledFor "FEATURE" myActor)

            result `shouldReturn` True

        it "returns True if the feature is enabled for the actor" $ do
            let feature = Feature { enabledEntities = [actorId myActor], enabledPercentage = 0, isEnabled = False }
            let featureState = Features $ M.insert "FEATURE" feature mempty
            let result = evalFlipperT featureState (enabledFor "FEATURE" myActor)

            result `shouldReturn` True

        it "returns False if the feature is not enabled for this actor" $ do
            let feature = Feature { enabledEntities = [], enabledPercentage = 0, isEnabled = False }
            let featureState = Features $ M.insert "FEATURE" feature mempty
            let result = evalFlipperT featureState (enabledFor "FEATURE" myActor)

            result `shouldReturn` False

    describe "disable" $ do
        it "disables a new feature key" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (disable "NEW_FEATURE" >> enabled "NEW_FEATURE")
            in
                result `shouldReturn` False

        it "disables a existing feature key" $
            let
                feature = def { isEnabled = True }
                featureState = Features $ M.insert "FEATURE" feature mempty
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
                feature = def { isEnabled = False }
                featureState = Features $ M.insert "FEATURE" feature mempty
                result = evalFlipperT featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` True

        it "disables a enabled feature key" $
            let
                feature = def { isEnabled = True }
                featureState = Features $ M.insert "FEATURE" feature mempty
                result = evalFlipperT featureState (toggle "FEATURE" >> enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "enabled" $ do
        it "returns True when the feature is enabled" $
            let
                feature = def { isEnabled = True }
                featureState = Features $ M.insert "ENABLED_FEATURE" feature mempty
                result = evalFlipperT featureState (enabled "ENABLED_FEATURE")
            in
                result `shouldReturn` True

        it "returns False when the feature is not enabled" $
            let
                feature = def { isEnabled = False }
                featureState = Features $ M.insert "DISABLED_FEATURE" feature mempty
                result = evalFlipperT featureState (enabled "DISABLED_FEATURE")
            in
                result `shouldReturn` False

        it "returns False when the feature key is not found" $
            let
                featureState = mempty :: Features
                result = evalFlipperT featureState (enabled "NON_EXISTANT_FEATURE")
            in
                result `shouldReturn` False

newtype TestActor = TestActor
    { myId :: Int
    } deriving (Show, Eq)

instance HasActorId TestActor where
    actorId actor = ActorId . C8.pack . show $ myId actor

myActor :: TestActor
myActor = TestActor 1

myOtherActor :: TestActor
myOtherActor = TestActor 2
