{-# OPTIONS_GHC -Wno-orphans #-}
module Blink.Generators () where

import Test.QuickCheck

import Blink.Geometry (Alignment, Insets (..), Point (..), Rectangle (..), Size (..))

coord :: Gen Double
coord = fromIntegral <$> (choose (-500, 500) :: Gen Int)

dimension :: Gen Double
dimension = fromIntegral <$> (choose (0, 500) :: Gen Int)

instance Arbitrary Point where
  arbitrary = Point <$> coord <*> coord

instance Arbitrary Size where
  arbitrary = Size <$> dimension <*> dimension

instance Arbitrary Rectangle where
  arbitrary = Rectangle <$> coord <*> coord <*> dimension <*> dimension

instance Arbitrary Insets where
  arbitrary = Insets <$> coord <*> coord <*> coord <*> coord

instance Arbitrary Alignment where
  arbitrary = arbitraryBoundedEnum
