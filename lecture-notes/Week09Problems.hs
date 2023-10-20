module Week09Problems where

-- FIXME: Pictures, using applicative functors

data RGBA =
  RGBA { redChannel :: Double
       , greenChannel :: Double
       , blueChannel :: Double
       , alphaChannel :: Double
       }
  deriving (Eq, Ord, Show)

red,green,blue :: RGBA
red = RGBA 1 0 0 1
green = RGBA 0 1 0 1
blue = RGBA 0 0 1 1

transparent :: RGBA
transparent = RGBA 0 0 0 0

opacity :: Double -> RGBA -> RGBA
opacity factor colour = colour { alphaChannel = factor * alphaChannel colour }

{------------------------------------------------------------------------------}
{- Points                                                                     -}

type Point = (Double, Double)

newtype Picture a = MkPicture (Point -> a)

everywhere :: a -> Picture a
everywhere a = MkPicture (\_ -> a)

circle :: Picture Bool
circle = MkPicture (\(x, y) -> x*x + y*y <= 1)

square :: Picture Bool
square = MkPicture (\(x,y) -> abs x <= 1 && abs y <= 1)

instance Semigroup a => Semigroup (Picture a) where
  MkPicture p1 <> MkPicture p2 =
    MkPicture (p1 <> p2)

instance Monoid a => Monoid (Picture a) where
  mempty = MkPicture mempty

instance Functor Picture where
  fmap f (MkPicture p) = MkPicture (f . p)

instance Applicative Picture where
  pure = everywhere
  MkPicture pf <*> MkPicture pa =
    MkPicture (\pt -> pf pt (pa pt))
