{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Fixed Point Polymorphic Arithmetic.

This module provides a type much like 'Data.Fixed.Fixed', but deviates in three ways:

- It's polymorphic in the base number type, rather than specialized to
  'Integer'. So you can use any 'Integral' number type you want, and also
  convert between them.
- It parameterize the resolution with 'Nat's directly, instead of using the
  'Data.Fixed.Resolution' class.
- The 'Num' and 'Fractional' operations round to the nearest value, rather than
  toward negative infinity.
-}

module Data.Fixed.Poly where

import           Control.Applicative (liftA2)
import           Data.Proxy (Proxy(Proxy))
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownNat, Nat, natVal)
import           Text.Read (Lexeme(Number, Symbol), choice, lexP, parens, readPrec)
import           Text.Read.Lex (numberToRational)

-- | Isomorphic to @n@, but with a 'Fractional' instance instead of an 'Integral' one.
type Uni   = Fixed 1
-- | Tenths.
type Deci  = Fixed 10
-- | Hundredths.
type Centi = Fixed 100
-- | Thousandths.
type Milli = Fixed 1000
-- | Millionths.
type Micro = Fixed 1000000
-- | Billionths / Milliardths.
type Nano  = Fixed 1000000000
-- | Trillionths / Billionths.
type Pico  = Fixed 1000000000000
-- | Quadrillionths / Billiardths.
type Femto = Fixed 1000000000000000
-- | Quintillionths / Trillionths.
type Atto  = Fixed 1000000000000000

-- | Change the resolution of a 'Fixed'.
changeResolution :: forall r r' n. (KnownNat r, KnownNat r', Integral n) => Fixed r n -> Fixed r' n
changeResolution (Fixed n) = let common = gcd (res @r) (res @r')
                                 numer = res @r' `div` common
                                 denom = res @r `div` common
                              in Fixed $ (n * numer + (denom `div` 2)) `div` denom

-- | Fixed point number, with a resolution set by @r@.
newtype Fixed (r :: Nat) a = Fixed a
  deriving (Bounded, Enum, Eq, Foldable, Functor, Generic, Ord, Storable, Traversable)

instance Applicative (Fixed r) where
  pure = Fixed
  Fixed f <*> a = f <$> a
  liftA2 f (Fixed a) (Fixed b) = Fixed $ f a b

instance Monad (Fixed r) where
  Fixed a >>= f = f a

-- | Helper to extract a resolution.
res :: forall r n. (KnownNat r, Num n) => n
res = fromInteger . natVal $ Proxy @r
{-# INLINE res #-}

-- | Helper to extract half of a resolution (used for round to nearest).
halfres :: forall r n. (KnownNat r, Integral n) => n
halfres = res @r `div` 2
{-# INLINE halfres #-}

-- | This Show instance is only valid for resolutions that are powers of 10.
instance (KnownNat r, Integral n, Show n) => Show (Fixed r n) where
  showsPrec prec (Fixed n) =
    let (whole, decimal) = n `quotRem` res @r
        sign = if length (show (signum whole) <> show (signum decimal)) > 2
                  then \s -> showParen (prec > 6) $ ('-':) . s else id
        decimal' = "." <> (tail . show $ res @r + abs decimal)
     in sign $ showsPrec 1 (abs whole) . showString decimal'

-- | This Read instance is only valid for fully specified numbers.
instance (KnownNat r, Integral n, Read n) => Read (Fixed r n) where
  readPrec = parens $ do
    sign <- choice [do Symbol "-" <- lexP; pure (-1), pure 1]
    Number n <- lexP
    pure . (sign *) . fromRational $ numberToRational n

instance (KnownNat r, Integral n) => Num (Fixed r n) where
  fromInteger = Fixed . (* res @r) . fromInteger
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  Fixed a * Fixed b = Fixed $ (a * b + halfres @r) `div` res @r
  negate = fmap negate
  abs = fmap abs
  signum = fmap ((res @r *) . signum)

instance (KnownNat r, Integral n) => Fractional (Fixed r n) where
  fromRational = Fixed . truncate . (0.5 +) . (res @r *)
  Fixed a / Fixed b = Fixed $ (a * res @r + (b `quot` 2)) `quot` b

instance (KnownNat r, Integral n) => Real (Fixed r n) where
  toRational (Fixed n) = fromIntegral n / res @r

instance (KnownNat r, Integral n) => RealFrac (Fixed r n) where
  properFraction (Fixed n) = (fromIntegral n `quot` res @r, Fixed $ n `rem` res @r)
