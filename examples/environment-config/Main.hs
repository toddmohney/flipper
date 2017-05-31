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
    -- load feature flags from the environment
    someFeatureEnabled <- maybe False read <$> Env.lookupEnv "SOME_FEATURE"
    someOtherFeatureEnabled <- maybe False read <$> Env.lookupEnv "SOME_OTHER_FEATURE"

    -- build flipper feature type
    pure . Features $ Map.fromList
        [ ("SOME_FEATURE", someFeatureEnabled)
        , ("SOME_OTHER_FEATURE", someOtherFeatureEnabled)
        ]
