{-|
Module      : Flipper
Description : Main user interface for the Flipper library
-}
module Control.Flipper
    ( enabled
    , enabledFor
    , enable
    , enableFor
    , disable
    , toggle
    , whenEnabled
    , whenEnabledFor
    , module Control.Flipper.Types
    ) where

import           Control.Monad         (when)
import           Data.Default          (def)
import           Data.Digest.CRC32     (CRC32)
import qualified Data.Digest.CRC32     as D
import           Data.Monoid           ((<>))

import           Control.Flipper.Types

{- |
The 'whenEnabled' function calls the supplied function, 'm ()', when the given
'FeatureName' is enabled.

When the feature specified by 'FeatureName' is disabled, 'm ()' is not
evaluated.
-}
whenEnabled :: (HasFeatureFlags m)
            => FeatureName -> m () -> m ()
whenEnabled fName f = do
    featureEnabled <- enabled fName
    when featureEnabled f

{- |
The 'whenEnabledFor' function calls the supplied function, 'm ()', when the given
'FeatureName' is enabled for the given entity.

When the feature specified by 'FeatureName' is disabled for the given entity,
'm ()' is not evaluated.
-}
whenEnabledFor :: (HasFeatureFlags m, CRC32 a)
               => FeatureName -> a -> m () -> m ()
whenEnabledFor fName entity f = do
    featureEnabled <- enabledFor fName entity
    when featureEnabled f

{- |
The 'enabled' function returns a Bool indicating if the queried feature is
active.

When the queried FeatureName exists, the active state is returned.

When the queried FeatureName does not exists, 'enabled' returns False.
-}
enabled :: HasFeatureFlags m
        => FeatureName -> m Bool
enabled fName = do
    mFeature <- getFeature fName
    case mFeature of
        (Just feature) -> return $ isEnabled feature
        Nothing        -> return False

{- |
The 'enabledFor' function returns a Bool indicating if the queried feature is
active for the given enitty.

If the queried FeatureName does not exists, 'enabledFor' returns False.
-}
enabledFor :: (HasFeatureFlags m, CRC32 a)
           => FeatureName -> a -> m Bool
enabledFor fName entity = do
    mFeature <- getFeature fName
    case mFeature of
        Nothing        -> return False
        (Just feature) -> return $ isEnabledFor feature entity

{- |
The 'enable' function activates a feature.

When the FeatureName exists in the store, it is set to active.

When the FeatureName does not exist, it is created and set to active.
-}
enable :: ModifiesFeatureFlags m
       => FeatureName -> m ()
enable fName = upsertFeature fName True

{- |
The 'enableFor' function activates a feature for a single entity.

If the FeatureName does not exist in the store, it is created and set to active
only for the given entity.
-}
enableFor :: (ModifiesFeatureFlags m, CRC32 a)
          => FeatureName -> a -> m ()
enableFor fName entity =
    update fName enableFor'
    where
        enableFor' :: (Maybe Feature -> Maybe Feature)
        enableFor' (Just feature) = Just $ feature
            { enabledEntities = enabledEntities feature <> [D.crc32 entity]
            }
        enableFor' Nothing = Just $ def
            { enabledEntities = mempty <> [D.crc32 entity]
            }

{- |
The 'disable' function deactivates a feature.

When the FeatureName exists in the store, it is set to inactive.

When the FeatureName does not exist, it is created and set to inactive.
-}
disable :: ModifiesFeatureFlags m
        => FeatureName -> m ()
disable fName = upsertFeature fName False

{- |
The 'toggle' function flips the current state of a feature.

When the FeatureName exists in the store, it flips the feature state.

When the FeatureName does not exist, it is created and set to True.
-}
toggle :: ModifiesFeatureFlags m
            => FeatureName -> m ()
toggle fName = update fName flipIt'
    where
        flipIt' :: Maybe Feature -> Maybe Feature
        flipIt' (Just feature) = Just (feature { isEnabled = not (isEnabled feature) })
        flipIt' Nothing  = Just $ def { isEnabled = True }

{- |
When the FeatureName exists in the store, it is set to the specified `isEnabled` state.

When the FeatureName does not exist, it is created and set to the specified `isEnabled` state.
-}
upsertFeature :: ModifiesFeatureFlags m
              => FeatureName -> Bool -> m ()
upsertFeature fName featureEnabled =
    update fName upsertFeature'
    where
        upsertFeature' :: (Maybe Feature -> Maybe Feature)
        upsertFeature' Nothing        = Just def { isEnabled = featureEnabled }
        upsertFeature' (Just feature) = Just (feature { isEnabled = featureEnabled })
