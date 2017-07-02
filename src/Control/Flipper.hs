{-|
Module      : Flipper
Description : Main user interface for the Flipper library
-}
module Control.Flipper
    ( enabled
    , enabledFor
    , enable
    , enableFor
    , enableForPercentage
    , disable
    , toggle
    , whenEnabled
    , whenEnabledFor
    , module Control.Flipper.Types
    ) where

import           Control.Monad         (when)
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
'FeatureName' is enabled for the given actor.

When the feature specified by 'FeatureName' is disabled for the given actor,
'm ()' is not evaluated.
-}
whenEnabledFor :: (HasFeatureFlags m, HasActorId a)
               => FeatureName -> a -> m () -> m ()
whenEnabledFor fName actor f = do
    featureEnabled <- enabledFor fName actor
    when featureEnabled f

{- |
The 'enabled' function returns a Bool indicating if the queried feature is
active.

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
enabledFor :: (HasFeatureFlags m, HasActorId a)
           => FeatureName -> a -> m Bool
enabledFor fName actor = do
    mFeature <- getFeature fName
    case mFeature of
        Nothing        -> return False
        (Just feature) -> return $ isEnabledFor feature actor

{- |
The 'enable' function activates a feature globally.

When the FeatureName does not exist, it is created and set to active.
-}
enable :: ModifiesFeatureFlags m
       => FeatureName -> m ()
enable fName = upsertFeature fName True

{- |
The 'enableFor' function activates a feature for a single actor.

If the FeatureName does not exist in the store, it is created and set to active
only for the given actor.
-}
enableFor :: (ModifiesFeatureFlags m, HasActorId a)
          => FeatureName -> a -> m ()
enableFor fName actor = update fName (enableFor' actor)

enableFor' :: HasActorId a => a -> Maybe Feature -> Maybe Feature
enableFor' actor (Just feature) = Just $ feature { enabledEntities = enabledEntities feature <> [actorId actor] }
enableFor' actor Nothing = Just $ mkFeature { enabledEntities = mempty <> [actorId actor] }

{- |
The 'enableForPercentage' function activates a feature for a percentage of actors.

If the FeatureName does not exist in the store, it is created and set to active
only for the specified percentage.
-}
enableForPercentage :: (ModifiesFeatureFlags m)
          => FeatureName -> Percentage -> m ()
enableForPercentage fName pct
    | pct < 0   = raiseOutOfRangeError
    | pct > 100 = raiseOutOfRangeError
    | otherwise = update fName (enableForPercentage' pct)
    where
        raiseOutOfRangeError = error ("Invalid percentage: " <> show pct <> " Expected a value between 0 - 100")

enableForPercentage' :: Percentage -> Maybe Feature -> Maybe Feature
enableForPercentage' pct (Just feature) = Just $ feature { enabledPercentage = pct }
enableForPercentage' pct Nothing = Just mkFeature { enabledPercentage = pct }

{- |
The 'disable' function deactivates a feature globally.

When the FeatureName does not exist, it is created and set to inactive.
-}
disable :: ModifiesFeatureFlags m
        => FeatureName -> m ()
disable fName = upsertFeature fName False

{- |
The 'toggle' function flips the current state of a feature globally.

When the FeatureName does not exist, it is created and set to True.
-}
toggle :: ModifiesFeatureFlags m
            => FeatureName -> m ()
toggle fName = update fName flipIt'
    where
        flipIt' :: Maybe Feature -> Maybe Feature
        flipIt' (Just feature) = Just (feature { isEnabled = not (isEnabled feature) })
        flipIt' Nothing  = Just $ mkFeature { isEnabled = True }

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
        upsertFeature' Nothing        = Just mkFeature { isEnabled = featureEnabled }
        upsertFeature' (Just feature) = Just (feature { isEnabled = featureEnabled })
