module Flipper
    ( module Flipper
    , module Flipper.Types
    ) where

import           Control.Monad   (void)
import qualified Data.Map.Strict as M

import           Flipper.Types   (FeatureName (..), Features (..),
                                  HasFeatureFlags (..), mkFeatures)

flipFeature :: HasFeatureFlags m
            => FeatureName -> m ()
flipFeature fName = update fName flipIt'
    where
        flipIt' :: Maybe Bool -> Maybe Bool
        flipIt' (Just a) = Just (not a)
        flipIt' Nothing  = Just True

enable :: HasFeatureFlags m
       => FeatureName -> m ()
enable fName = update fName (\_ -> Just True)

enabled :: HasFeatureFlags m
        => FeatureName -> m Bool
enabled fName = do
    features <- unFeatures <$> getFeatures
    if M.lookup fName features == Just True
        then return True
        else return False

disable :: HasFeatureFlags m
        => FeatureName -> m ()
disable fName = update fName (\_ -> Just False)

update :: HasFeatureFlags m
       => FeatureName -> (Maybe Bool -> Maybe Bool) -> m ()
update fName updateFn = do
    features <- unFeatures <$> getFeatures
    void . updateFeatures . Features $ M.alter updateFn fName features
