{-|
Module      : Flipper
Description : Main user interface for the Flipper library
-}
module Control.Flipper
    ( enabled
    , enable
    , disable
    , toggle
    , whenEnabled
    , module Control.Flipper.Types
    ) where

import           Control.Monad         (when)

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
        Nothing -> return False

{- |
The 'enable' function activates a feature.

When the FeatureName exists in the store, it is set to active.

When the FeatureName does not exist, it is created and set to active.
-}
enable :: ModifiesFeatureFlags m
       => FeatureName -> m ()
enable fName = upsertFeature fName True

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
        flipIt' Nothing  = Just Feature { isEnabled = True }

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
        upsertFeature' Nothing        = Just Feature { isEnabled = featureEnabled }
        upsertFeature' (Just feature) = Just (feature { isEnabled = featureEnabled })
