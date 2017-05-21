module Flipper
    ( enabled
    , enable
    , disable
    , toggle
    , module Flipper.Types
    ) where

import           Control.Monad   (void)
import qualified Data.Map.Strict as M

import           Flipper.Types   (FeatureName (..), Features (..),
                                  HasFeatureFlags (..), mkFeatures)

{- |
The 'enabled' returns a Bool indicating if the queried feature is active.

When the queried FeatureName exists, the active state is returned.

When the queried FeatureName does not exists, 'enabled' returns False.
-}
enabled :: HasFeatureFlags m
        => FeatureName -> m Bool
enabled fName = do
    features <- unFeatures <$> getFeatures
    if M.lookup fName features == Just True
        then return True
        else return False

{- |
The 'enable' function activates a feature.

When the FeatureName exists in the store, it is set to active.

When the FeatureName does not exist, it is created and set to active.
-}
enable :: HasFeatureFlags m
       => FeatureName -> m ()
enable fName = update fName (\_ -> Just True)

{- |
The 'disable' function deactivates a feature.

When the FeatureName exists in the store, it is set to inactive.

When the FeatureName does not exist, it is created and set to inactive.
-}
disable :: HasFeatureFlags m
        => FeatureName -> m ()
disable fName = update fName (\_ -> Just False)

{- |
The 'toggle' function flips the current state of a feature.

When the FeatureName exists in the store, it flips the feature state.

When the FeatureName does not exist, it is created and set to True.
-}
toggle :: HasFeatureFlags m
            => FeatureName -> m ()
toggle fName = update fName flipIt'
    where
        flipIt' :: Maybe Bool -> Maybe Bool
        flipIt' (Just a) = Just (not a)
        flipIt' Nothing  = Just True

update :: HasFeatureFlags m
       => FeatureName -> (Maybe Bool -> Maybe Bool) -> m ()
update fName updateFn = do
    features <- unFeatures <$> getFeatures
    void . updateFeatures . Features $ M.alter updateFn fName features
