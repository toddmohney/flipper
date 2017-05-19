module Flipper.Types
    ( Features(..)
    , FeatureName(..)
    , mkFeatures
    ) where

import           Data.Map.Strict     (Map)
import           Data.Monoid
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import qualified Data.Text           as T

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
