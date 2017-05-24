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
    isEnabled <- enabled fName
    when isEnabled f

{- |
The 'enabled' function returns a Bool indicating if the queried feature is
active.

When the queried FeatureName exists, the active state is returned.

When the queried FeatureName does not exists, 'enabled' returns False.
-}
enabled :: HasFeatureFlags m
        => FeatureName -> m Bool
enabled fName = do
    feature <- getFeature fName
    if feature == Just True
        then return True
        else return False

{- |
The 'enable' function activates a feature.

When the FeatureName exists in the store, it is set to active.

When the FeatureName does not exist, it is created and set to active.
-}
enable :: ModifiesFeatureFlags m
       => FeatureName -> m ()
enable fName = updateFeature fName True

{- |
The 'disable' function deactivates a feature.

When the FeatureName exists in the store, it is set to inactive.

When the FeatureName does not exist, it is created and set to inactive.
-}
disable :: ModifiesFeatureFlags m
        => FeatureName -> m ()
disable fName = updateFeature fName False

{- |
The 'toggle' function flips the current state of a feature.

When the FeatureName exists in the store, it flips the feature state.

When the FeatureName does not exist, it is created and set to True.
-}
toggle :: ModifiesFeatureFlags m
            => FeatureName -> m ()
toggle fName = update fName flipIt'
    where
        flipIt' :: Maybe Bool -> Maybe Bool
        flipIt' (Just a) = Just (not a)
        flipIt' Nothing  = Just True
