module Control.FlipperSpec (main, spec) where

import qualified Data.ByteString.Char8           as C8
import qualified Data.Map.Strict                 as M
import qualified Data.Set as S
import           Test.Hspec

import Control.Flipper
    ( ActorId(..)
    , Features(..)
    , Feature(..)
    , HasActorId(..)
    )
import qualified Control.Flipper as F
import qualified Control.Flipper.Adapters.Memory as FM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "whenEnabled" $ do
        context "the feature is enabled" $
            it "evaluates the given monad" $ do
                let feature = (F.mkFeature "ADD_ANOTHER_FEATURE") { isEnabled = True }
                let featureState = Features $ M.singleton (featureName feature) feature
                store <- FM.execFlipperT featureState (F.whenEnabled "ADD_ANOTHER_FEATURE" (F.enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (F.mkFeature "ANOTHER_FEATURE") { isEnabled = True }

        context "the feature is disabled" $
            it "does not evaluate the given monad" $ do
                let feature = (F.mkFeature "ADD_ANOTHER_FEATURE") { isEnabled = False }
                let featureState = Features $ M.singleton (featureName feature) feature
                store <- FM.execFlipperT featureState (F.whenEnabled "ADD_ANOTHER_FEATURE" (F.enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Nothing

    describe "whenEnabledFor" $ do
        context "the feature is enabled globally" $
            it "evaluates the given monad" $ do
                let feature = (F.mkFeature "ADD_ANOTHER_FEATURE") { isEnabled = True }
                let featureState = Features $ M.singleton (featureName feature) feature
                store <- FM.execFlipperT featureState (F.whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (F.enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (F.mkFeature "ANOTHER_FEATURE") { isEnabled = True }

        context "the feature is enabled for the given actor" $
            it "evaluates the given monad" $ do
                let feature = (F.mkFeature "ADD_ANOTHER_FEATURE") { enabledActors = S.singleton (actorId myActor), isEnabled = False }
                let featureState = Features $ M.singleton (featureName feature) feature
                store <- FM.execFlipperT featureState (F.whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (F.enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Just (F.mkFeature "ANOTHER_FEATURE") { isEnabled = True }

        context "the feature is not enabled for the given actor" $
            it "does not evaluate the given monad" $ do
                let feature = F.mkFeature "ADD_ANOTHER_FEATURE"
                let featureState = Features $ M.singleton (featureName feature) feature
                store <- FM.execFlipperT featureState (F.whenEnabledFor "ADD_ANOTHER_FEATURE" myActor (F.enable "ANOTHER_FEATURE"))
                M.lookup "ANOTHER_FEATURE" (unFeatures store) `shouldBe` Nothing

    describe "enable" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = FM.evalFlipperT featureState (F.enable "NEW_FEATURE" >> F.enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a existing feature key" $
            let
                feature = (F.mkFeature "FEATURE") { isEnabled = False }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.enable "FEATURE" >> F.enabled "FEATURE")
            in
                result `shouldReturn` True

    describe "enableFor" $ do
        describe "with an existing feature" $ do
            it "enables the feature only for the given actor" $ do
                let feature = F.mkFeature "FEATURE"
                let featureState = Features $ M.singleton (featureName feature) feature
                let result1 = FM.evalFlipperT featureState (F.enableFor "FEATURE" myActor >> F.enabledFor "FEATURE" myActor)
                let result2 = FM.evalFlipperT featureState (F.enableFor "FEATURE" myActor >> F.enabledFor "FEATURE" myOtherActor)

                result1 `shouldReturn` True
                result2 `shouldReturn` False

        describe "with a non-existant feature" $ do
            it "enables the feature only for the given actor" $ do
                let featureState = mempty :: Features
                let result1 = FM.evalFlipperT featureState (F.enableFor "FEATURE" myActor >> F.enabledFor "FEATURE" myActor)
                let result2 = FM.evalFlipperT featureState (F.enableFor "FEATURE" myActor >> F.enabledFor "FEATURE" myOtherActor)

                result1 `shouldReturn` True
                result2 `shouldReturn` False

    describe "enabledFor" $ do
        it "returns True if the feature is enabled globally" $ do
            let feature = (F.mkFeature "FEATURE") { isEnabled = True }
            let featureState = Features $ M.singleton (featureName feature) feature
            let result = FM.evalFlipperT featureState (F.enabledFor "FEATURE" myActor)

            result `shouldReturn` True

        it "returns True if the feature is enabled for the actor" $ do
            let feature = (F.mkFeature "FEATURE") { enabledActors = S.singleton (actorId myActor), isEnabled = False }
            let featureState = Features $ M.singleton (featureName feature) feature
            let result = FM.evalFlipperT featureState (F.enabledFor "FEATURE" myActor)

            result `shouldReturn` True

        it "returns False if the feature is not enabled for this actor" $ do
            let feature = F.mkFeature "FEATURE"
            let featureState = Features $ M.singleton (featureName feature) feature
            let result = FM.evalFlipperT featureState (F.enabledFor "FEATURE" myActor)

            result `shouldReturn` False

    describe "disable" $ do
        it "disables a new feature key" $
            let
                featureState = mempty :: Features
                result = FM.evalFlipperT featureState (F.disable "NEW_FEATURE" >> F.enabled "NEW_FEATURE")
            in
                result `shouldReturn` False

        it "disables a existing feature key" $
            let
                feature = (F.mkFeature "FEATURE") { isEnabled = True }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.disable "FEATURE" >> F.enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "toggle" $ do
        it "enables a new feature key" $
            let
                featureState = mempty :: Features
                result = FM.evalFlipperT featureState (F.toggle "NEW_FEATURE" >> F.enabled "NEW_FEATURE")
            in
                result `shouldReturn` True

        it "enables a disabled feature key" $
            let
                feature = (F.mkFeature "FEATURE") { isEnabled = False }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.toggle "FEATURE" >> F.enabled "FEATURE")
            in
                result `shouldReturn` True

        it "disables a enabled feature key" $
            let
                feature = (F.mkFeature "FEATURE") { isEnabled = True }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.toggle "FEATURE" >> F.enabled "FEATURE")
            in
                result `shouldReturn` False

    describe "enabled" $ do
        it "returns True when the feature is enabled" $
            let
                feature = (F.mkFeature "ENABLED_FEATURE") { isEnabled = True }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.enabled "ENABLED_FEATURE")
            in
                result `shouldReturn` True

        it "returns False when the feature is not enabled" $
            let
                feature = (F.mkFeature "DISABLED_FEATURE") { isEnabled = False }
                featureState = Features $ M.singleton (featureName feature) feature
                result = FM.evalFlipperT featureState (F.enabled "DISABLED_FEATURE")
            in
                result `shouldReturn` False

        it "returns False when the feature key is not found" $
            let
                featureState = mempty :: Features
                result = FM.evalFlipperT featureState (F.enabled "NON_EXISTANT_FEATURE")
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
