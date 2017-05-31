{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Control.Flipper.Types
Description : Datatype and Typeclass definitions
-}
module Control.Flipper.Types
    ( Features(..)
    , FeatureName(..)
    , HasFeatureFlags(..)
    , ModifiesFeatureFlags(..)
    , update
    , mkFeatures
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
    getFeature :: FeatureName -> m (Maybe Bool)

    default getFeature :: (MonadTrans t, HasFeatureFlags m1, m ~ t m1) => FeatureName -> m (Maybe Bool)
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
    updateFeature :: FeatureName -> Bool -> m ()

    default updateFeature :: (MonadTrans t, ModifiesFeatureFlags m1, m ~ t m1) => FeatureName -> Bool -> m ()
    updateFeature fName isEnabled = lift $ updateFeature fName isEnabled

instance (MonadIO m, ModifiesFeatureFlags m) => ModifiesFeatureFlags (StateT s m)
instance (MonadIO m, ModifiesFeatureFlags m) => ModifiesFeatureFlags (ReaderT s m)

{- |
An abstraction representing the current state of the features store.
-}
newtype Features = Features { unFeatures :: Map FeatureName Bool }
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
Convienience constructor
-}
mkFeatures :: Map FeatureName Bool -> Features
mkFeatures = Features

{- |
Updates a single Feature within the current monad
-}
update :: ModifiesFeatureFlags m
       => FeatureName -> (Maybe Bool -> Maybe Bool) -> m ()
update fName updateFn = do
    features <- unFeatures <$> getFeatures
    void . updateFeatures . Features $ Map.alter updateFn fName features
