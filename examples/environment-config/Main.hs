module Main where

import Control.Monad (when)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import qualified System.Environment as Env

import Flipper

main :: IO ()
main = do
    features <- loadFeatures

    -- StateT has a HasFeatureFlags instance defined
    evalStateT runWithFeatureFlags features

runWithFeatureFlags :: (MonadIO m, HasFeatureFlags m)
                    => m ()
runWithFeatureFlags = do
    stuffEnabled <- enabled "ENABLE_STUFF"
    thingsEnabled <- enabled "ENABLE_THINGS"

    when
        stuffEnabled
        (liftIO $ putStrLn "We're running STUFF!")

    when
        thingsEnabled
        (liftIO $ putStrLn "We're running THINGS!")

    liftIO $ putStrLn "Done!"

loadFeatures :: IO Features
loadFeatures = do
    -- load feature flags from the environment
    doStuffEnabled <- maybe False read <$> Env.lookupEnv "ENABLE_STUFF"
    doThingsEnabled <- maybe False read <$> Env.lookupEnv "ENABLE_THINGS"

    -- build flipper feature type
    pure . Features $ Map.fromList
        [ ("ENABLE_STUFF", doStuffEnabled)
        , ("ENABLE_THINGS", doThingsEnabled)
        ]
