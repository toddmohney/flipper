{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Flipper.Adapters.Memory
    ( FlipperT(..)
    , evalFlipperT
    , execFlipperT
    , runFlipperT
    ) where

import           Control.Monad.State
import qualified Data.Map.Strict       as Map

import           Control.Flipper.Types

{- |
The 'FlipperT' transformer for in-memory feature switchable computation.
-}
newtype FlipperT m a = FlipperT { unFlipperT :: StateT Features m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState Features
             , MonadTrans
             )

instance (Monad m) => HasFeatureFlags (FlipperT m) where
    getFeatures = get

    getFeature featureName = do
        features <- unFeatures <$> getFeatures
        return (Map.lookup featureName features)

instance (Monad m) => ModifiesFeatureFlags (FlipperT m) where
    updateFeatures = put

    updateFeature featureName feature  = update featureName (\_ -> Just feature)

{- |
Evaluates a feature-switched computation, returning the final value and
discarding the final state of the feature switches.
-}
evalFlipperT :: (Monad m) => Features -> FlipperT m a -> m a
evalFlipperT features f = evalStateT (unFlipperT f) features

{- |
Executes a feature-switched computation, returning the final state of the
feature switches, discarding the final value of the computation.
-}
execFlipperT :: (Monad m) => Features -> FlipperT m a -> m Features
execFlipperT features f = execStateT (unFlipperT f) features

{- |
Runs a feature-switched computation, returning the final value and state of the
feature switches.
-}
runFlipperT :: (Monad m) => Features -> FlipperT m a -> m (a, Features)
runFlipperT features f = runStateT (unFlipperT f) features
