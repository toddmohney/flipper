module Main where

import           Control.Monad.State
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict                 as Map
import           Data.Digest.CRC32 (CRC32)
import qualified Data.Digest.CRC32 as D

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
        [ ("SOME_FEATURE", disabledFeature)
        , ("SOME_GLOBALLY_ENABLED_FEATURE", enabledFeature)
        ]
    where
        disabledFeature :: Feature
        disabledFeature = Feature
            { isEnabled = False
            , enabledEntities = []
            }

        enabledFeature :: Feature
        enabledFeature = Feature
            { isEnabled = True
            , enabledEntities = []
            }

data User = User { myId :: Int }
    deriving (Show)

{-
 - An instance of CRC32 is required here to keep track of the user in the
 - Feature's list of enabled entities.
 -}
instance CRC32 User where
    crc32Update seed user =
        D.crc32Update seed (C8.pack . show $ myId user)

myUser :: User
myUser = User 1

myOtherUser :: User
myOtherUser = User 2
