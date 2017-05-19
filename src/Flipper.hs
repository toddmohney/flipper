module Flipper
    ( enable
    , enabled
    , disable
    , flipFeature
    , mkFeatures
    ) where

import           Control.Monad       (void)
import           Control.Monad.State (State, get, put)
import           Data.Map.Strict     (Map (..))
import qualified Data.Map.Strict     as M
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


flipFeature :: FeatureName -> State Features ()
flipFeature fName = update fName flipIt'
    where
        flipIt' :: Maybe Bool -> Maybe Bool
        flipIt' (Just a) = Just (not a)
        flipIt' Nothing = Just True


enable :: FeatureName -> State Features ()
enable fName = update fName (\_ -> Just True)


enabled :: FeatureName -> State Features Bool
enabled fName = do
    features <- unFeatures <$> get
    if M.lookup fName features == Just True
        then return True
        else return False


disable :: FeatureName -> State Features ()
disable fName = update fName (\_ -> Just False)


mkFeatures :: Map FeatureName Bool -> Features
mkFeatures = Features


update :: FeatureName -> (Maybe Bool -> Maybe Bool) -> State Features ()
update fName updateFn = do
    features <- unFeatures <$> get
    void . put . Features $ M.alter updateFn fName features
