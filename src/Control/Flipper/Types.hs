{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : Flipper.Types
Description : Datatype and Typeclass definitions
-}
module Control.Flipper.Types
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

{- |
The 'HasFeatureFlags' typeclass describes how to access and modify the Features
store within the current monad.
-}
class Monad m => HasFeatureFlags m where
    -- | 'getFeatures' access the Features store within the current monad
    getFeatures :: m Features
    -- | 'updateFeatures' modifies the Features store within the current monad
    updateFeatures :: Features -> m ()

instance (Monad m) => HasFeatureFlags (StateT Features m) where
    getFeatures = get
    updateFeatures = put

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
