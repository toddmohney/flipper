{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Control.Flipper.Types
Description : Datatype and Typeclass definitions
-}
module Control.Flipper.Types
    ( Feature(..)
    , Features(..)
    , FeatureName(..)
    , HasFeatureFlags(..)
    , ModifiesFeatureFlags(..)
    , update
    ) where

import           Control.Monad   (void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T

{- |
The 'HasFeatureFlags' typeclass describes how to access the Features store
within the current monad.
-}
class Monad m => HasFeatureFlags m where
    -- | 'getFeatures' access the Features store within the current monad
    getFeatures :: m Features

    default getFeatures :: (MonadTrans t, HasFeatureFlags m1, m ~ t m1) => m Features
    getFeatures = lift getFeatures

    -- | 'getFeature' access a single Feature within the current monad
    getFeature :: FeatureName -> m (Maybe Feature)

    default getFeature :: (MonadTrans t, HasFeatureFlags m1, m ~ t m1) => FeatureName -> m (Maybe Feature)
    getFeature = lift . getFeature

instance (MonadIO m, HasFeatureFlags m) => HasFeatureFlags (StateT s m)
instance (MonadIO m, HasFeatureFlags m) => HasFeatureFlags (ReaderT s m)

{- |
The 'ModifiesFeatureFlags' typeclass describes how to modify the Features store
within the current monad.
-}
class HasFeatureFlags m => ModifiesFeatureFlags m where
    -- | 'updateFeatures' modifies the Features store within the current monad
    updateFeatures :: Features -> m ()

    default updateFeatures :: (MonadTrans t, ModifiesFeatureFlags m1, m ~ t m1) => Features -> m ()
    updateFeatures = lift . updateFeatures

    -- | 'updateFeature' modifies a single Feature within the current monad
    updateFeature :: FeatureName -> Feature -> m ()

    default updateFeature :: (MonadTrans t, ModifiesFeatureFlags m1, m ~ t m1) => FeatureName -> Feature -> m ()
    updateFeature fName feature = lift $ updateFeature fName feature

instance (MonadIO m, ModifiesFeatureFlags m) => ModifiesFeatureFlags (StateT s m)
instance (MonadIO m, ModifiesFeatureFlags m) => ModifiesFeatureFlags (ReaderT s m)

newtype Feature = Feature
    { isEnabled :: Bool
    } deriving (Show, Eq)

{- |
An abstraction representing the current state of the features store.
-}
newtype Features = Features { unFeatures :: Map FeatureName Feature }
    deriving (Show, Eq)

instance Monoid Features where
    mempty = Features mempty
    mappend a b = Features (unFeatures a <> unFeatures b)

{- |
The main identifier of a feature
-}
newtype FeatureName = FeatureName { unFeatureName :: Text }
    deriving (Show, Eq, Ord)

instance IsString FeatureName where
    fromString s = FeatureName (T.pack s)

{- |
Updates a single Feature within the current monad
-}
update :: ModifiesFeatureFlags m
       => FeatureName -> (Maybe Feature -> Maybe Feature) -> m ()
update fName updateFn = do
    features <- unFeatures <$> getFeatures
    void . updateFeatures . Features $ Map.alter updateFn fName features
