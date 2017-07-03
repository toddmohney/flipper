module Main where

import           Control.Monad.State
import qualified Data.ByteString.Char8           as C8
import qualified Data.Map.Strict                 as Map

import           Control.Flipper
import           Control.Flipper.Adapters.Memory

main :: IO ()
main = do
    features <- loadFeatures

    -- StateT has a HasFeatureFlags instance defined
    evalFlipperT features runWithFeatureFlags

runWithFeatureFlags :: (MonadIO m, ModifiesFeatureFlags m)
                    => m ()
runWithFeatureFlags = do
    -- we'll enabled SOME_FEATURE for myUser, but not myOtherUser
    enableFor "SOME_FEATURE" myUser

    -- we'll see this feature get executed for myUser
    whenEnabledFor "SOME_FEATURE" myUser
        (liftIO . putStrLn $ "We're running SOME_FEATURE for " ++ show myUser)

    -- we won't see this feature get executed because it has not been
    -- enabled for myOtherUser
    whenEnabledFor "SOME_FEATURE" myOtherUser
        (liftIO . putStrLn $ "We're running SOME_FEATURE for " ++ show myOtherUser)

    -- when a feature is globally enabled, the `whenEnabledfor` guard will
    -- execute the feature for all users.
    whenEnabledFor "SOME_GLOBALLY_ENABLED_FEATURE" myUser
        (liftIO . putStrLn $ "We're running SOME_GLOBALLY_ENABLED_FEATURE for " ++ show myUser)
    whenEnabledFor "SOME_GLOBALLY_ENABLED_FEATURE" myOtherUser
        (liftIO . putStrLn $ "We're running SOME_GLOBALLY_ENABLED_FEATURE for " ++ show myOtherUser)

    liftIO $ putStrLn "Done!"

loadFeatures :: IO Features
loadFeatures =
    -- build flipper feature type
    pure . Features $ Map.fromList
        [ (featureName disabledFeature, disabledFeature)
        , (featureName enabledFeature, enabledFeature)
        ]
    where
        disabledFeature :: Feature
        disabledFeature = mkFeature "SOME_FEATURE"

        enabledFeature :: Feature
        enabledFeature = (mkFeature "SOME_GLOBALLY_ENABLED_FEATURE") { isEnabled = True }

data User = User { myId :: Int }
    deriving (Show)

{-
 - An instance of HasActorId is required here to keep track of the user in the
 - Feature's list of enabled actors.
 -}
instance HasActorId User where
    actorId user = ActorId . C8.pack . show $ myId user

myUser :: User
myUser = User 1

myOtherUser :: User
myOtherUser = User 2
