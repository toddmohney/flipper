{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Flipper.Types
    ( Features(..)
    , FeatureName(..)
    , HasFeatureFlags(..)
    , mkFeatures
    ) where

import           Control.Monad.State (StateT, get, put)
import           Data.Map.Strict     (Map)
import           Data.Monoid
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import qualified Data.Text           as T


class Monad m => HasFeatureFlags m where
    getFeatures :: m Features
    updateFeatures :: Features -> m ()

instance (Monad m) => HasFeatureFlags (StateT Features m) where
    getFeatures = get
    updateFeatures = put

newtype Features = Features { unFeatures :: Map FeatureName Bool }
    deriving (Show, Eq)

instance Monoid Features where
    mempty = Features mempty
    mappend a b = Features (unFeatures a <> unFeatures b)

newtype FeatureName = FeatureName { unFeatureName :: Text }
    deriving (Show, Eq, Ord)

instance IsString FeatureName where
    fromString s = FeatureName (T.pack s)

mkFeatures :: Map FeatureName Bool -> Features
mkFeatures = Features
