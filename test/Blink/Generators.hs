{-# OPTIONS_GHC -Wno-orphans #-}
module Blink.Generators (genPointIn) where

import Test.QuickCheck

import Blink.Geometry (Alignment, Insets (..), Point (..), Rectangle (..), Size (..))
import Blink.Layout (Layout (..), Length (..))

coord :: Gen Double
coord = fromIntegral <$> (choose (-500, 500) :: Gen Int)

dimension :: Gen Double
dimension = fromIntegral <$> (choose (0, 500) :: Gen Int)

-- | A point chosen uniformly at random anywhere within (and including the
-- edges of) the given rectangle -- for a behaviour that should hold no
-- matter where within a region it's exercised, rather than just at one
-- fixed, hand-picked spot (e.g. a control whose hit area spans more than
-- one visually distinct part, like a checkbox's glyph and caption).
genPointIn :: Rectangle -> Gen Point
genPointIn r = Point
  <$> choose (rectX r, rectX r + rectWidth r)
  <*> choose (rectY r, rectY r + rectHeight r)

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

instance Arbitrary Length where
  arbitrary = oneof
    [ Exactly <$> dimension
    , pure Fill
    , AtLeast <$> dimension
    , AtMost  <$> dimension
    , (\lo d -> Between lo (lo + d)) <$> dimension <*> dimension
    ]

instance Arbitrary Layout where
  arbitrary = Layout <$> arbitrary <*> arbitrary <*> arbitrary
