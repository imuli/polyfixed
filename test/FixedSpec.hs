{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module FixedSpec
  ( spec
  ) where

import           Data.Foldable (for_)
import           Data.Proxy (Proxy(Proxy))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Classes (Laws(Laws), boundedEnumLaws, eqLaws, lawsProperties,
                     lawsTypeclass, numLaws, ordLaws, showLaws, showReadLaws)

import           Data.Fixed.Poly

instance (Arbitrary n) => Arbitrary (Fixed r n) where
  arbitrary = pure <$> arbitrary

checkLaws :: Laws -> Spec
checkLaws Laws{..} = describe lawsTypeclass $ for_ lawsProperties $ uncurry prop

spec :: Spec
spec = do
  checkLaws $ boundedEnumLaws $ Proxy @(Femto Int)
  checkLaws $ eqLaws $ Proxy @(Micro Int)
  checkLaws $ numLaws $ Proxy @(Femto Integer)
  checkLaws $ ordLaws $ Proxy @(Micro Int)
  checkLaws $ showLaws $ Proxy @(Micro Int)
  checkLaws $ showReadLaws $ Proxy @(Micro Int)
