module Main where

import           Control.Monad.State
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (maybe)
import qualified System.Environment              as Env

import           Control.Flipper
import           Control.Flipper.Adapters.Memory

main :: IO ()
main = do
    features <- loadFeatures

    -- StateT has a HasFeatureFlags instance defined
    evalFlipperT features runWithFeatureFlags

runWithFeatureFlags :: (MonadIO m, HasFeatureFlags m)
                    => m ()
runWithFeatureFlags = do
    whenEnabled
        "SOME_FEATURE"
        (liftIO $ putStrLn "We're running SOME_FEATURE!")

    whenEnabled
        "SOME_OTHER_FEATURE"
        (liftIO $ putStrLn "We're running SOME_OTHER_FEATURE!")

    liftIO $ putStrLn "Done!"

loadFeatures :: IO Features
loadFeatures = do
    let someFeature = mkFeature "SOME_FEATURE"
    let someOtherFeature = mkFeature "SOME_OTHER_FEATURE"
    -- load feature flags from the environment
    -- use the default values if the key is not found in ENV
    --   - features default to their 'disabled' state
    someFeatureEnabled      <- maybe someFeature (\val -> someFeature { isEnabled = read val }) <$> Env.lookupEnv "SOME_FEATURE"
    someOtherFeatureEnabled <- maybe someOtherFeature (\val -> someOtherFeature { isEnabled = read val }) <$> Env.lookupEnv "SOME_OTHER_FEATURE"

    -- build flipper feature type
    pure . Features $ Map.fromList
        [ (featureName someFeatureEnabled, someFeatureEnabled)
        , (featureName someOtherFeatureEnabled, someOtherFeatureEnabled)
        ]
