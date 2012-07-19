import Data.Generics
import Data.Data
import Data.Typeable

data O               dddz dz dr dg db = O dddz
data R                    dz  r  g  b = R   dz r
data G                    dz  r  g  b = G   dz g
data B                    dz  r  g  b = B   dz b
data C  c  m  y  ddk                  = C  ddk c
data M  c  m  y  ddk                  = M  ddk m
data Y  c  m  y  ddk                  = Y  ddk y
data I dc dm dy  ddk dddk             = I dddk
--     1  2   3   4   5   6   7  8  9


--     456 7 9 012 
--     .*. .*. .*.  [-/+]
--     .*. .*. .*.  [+/-]
--     .*. .*. .*.  [-/+]
--     456 7 9 012
--          8

-- This is the same grid as on page 37..41, but without the numeric labels for the cells
--
-- and with numeric labels for the columns
-- the d's are supposed to represent remaining choices (without specifying if they are binary choices)
--
-- the first  choice is to adjoin among the 3     columns to place  the 8 (= "infinity")
-- the second choice is to choose among the 3 sub-columns to obtain the three "dozens" (yielding three disjoint sets of size 8)

-- http://arxiv.org/pdf/math/0609449v2.pdf

infix 5 <$>

class ColorFunctor f where
     (<$>) :: Iso2 a b -> f a -> f b

data ZRGB  o r g b   = forall z. Z  (O o z r g b  )                   [(R z r g b)] [(G z r g b)] [(B z r g b)]
data CMYK    y m c i = forall k. K  (I   c m y k i)                   [(C c m y k)] [(M c m y k)] [(Y c m y k)]
data ZRGB2 o r g b   = forall z. Z2 (O         (I o o o o o) z r g b) [(R z r g b)] [(G z r g b)] [(B z r g b)]
data CMYK2   y m c i = forall k. K2 (I c m y k (O i i i i i)        ) [(C c m y k)] [(M c m y k)] [(Y c m y k)]
data ZRGB1   r g b   = forall z. Z1                                   [(R z r g b)] [(G z r g b)] [(B z r g b)]
data CMYK1   y m c   = forall k. K1                                   [(C c m y k)] [(M c m y k)] [(Y c m y k)]
data ZRGB0 o r g b   = forall z. Z0                                   [(R z r g b)] [(G z r g b)] [(B z r g b)]
data CMYK0   y m c i = forall k. K0                                   [(C c m y k)] [(M c m y k)] [(Y c m y k)]


data Iso0  zero one zRGB cmyK = forall r g b c m y.
     Iso0 (zero     zRGB               r g b       -> Maybe (CMYK0      y m c one))
              (one      cmyK                c m y  -> Maybe (ZRGB0 zero r g b    ))


data InvIso2 zRGB cmyK =  forall oi io x y z.
     InvIso2 ((cmyK oi x y z) -> Maybe (zRGB io x y z))
             ((zRGB io x y z) -> Maybe (cmyK oi x y z))

data Iso2  zRGB  cmyK = forall           oi io  r g b y m c.
     Iso2 (zRGB           -> Maybe (CMYK2 oi           y m c  ))
          (      cmyK     -> Maybe (ZRGB2    io  r g b        ))


data Iso zRGB  cmyK = forall r g b c m y.
     Iso (     zRGB               r g b       -> Maybe (CMYK1      y m c))
              (     cmyK               c m y -> Maybe (ZRGB1 r g b    ))
   


-- ^
-- >>> :k Iso CMYK ZRGB
-- Iso CMYK ZRGB :: * -> AnyK -> *

-- ^
-- >>> :k Iso ZRGB CMYK
-- Iso ZRGB CMYK :: AnyK -> * -> *

-- ^
-- >>> :k Iso ZRGB ZRGB
-- Iso ZRGB ZRGB :: AnyK -> AnyK -> *

-- ^
-- >>> :k Iso CMYK CMYK
-- Iso CMYK CMYK :: * -> * -> *

