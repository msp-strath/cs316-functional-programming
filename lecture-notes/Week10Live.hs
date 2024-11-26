------------------------------------------------------------------------
-- Advanced Functional Programming

-- This is a taster for CS410 next year.
-- However I thought it would be too unfair to jump straight to Agda
-- and be like "look at all the amazing things we can do in this
-- totally different language".

-- So instead, let's do something advanced but in a somewhat painful
-- manner in Haskell!

------------------------------------------------------------------------
-- Some spicy language extensions (there's tons more!)

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Week10Live where

import Data.Kind (Type)

------------------------------------------------------------------------
-- Deriving magic

-- A random data definition
-- Doesn't (o () o) look like a bird facing you?
data Bird o = MkBird o () o
  deriving (Show, Functor)

-- The purpose of this lecture is to look at the `deriving Functor`
-- part and figure out how we could build our own if we needed to.

bird15 :: Bird Int
bird15 = (1+) <$> MkBird 0 () 4

------------------------------------------------------------------------
-- Building blocks: Cst, Prd, Idt

newtype Cst a f i = MkCst { runCst :: a } deriving Show
instance Functor (Cst a f) where
  fmap f (MkCst v) = MkCst v

newtype Prd k l f i = MkPrd { runPrd :: (k f i, l f i) } deriving Show
instance (Functor (k f), Functor (l f)) => Functor (Prd k l f) where
  fmap f (MkPrd (v, w)) = MkPrd (fmap f v, fmap f w)

newtype Idt f i = MkIdt { runIdt :: i } deriving Show
instance Functor (Idt f) where
  fmap f (MkIdt v) = MkIdt (f v)

------------------------------------------------------------------------
-- Conversion

type CodeBird = Prd Idt (Prd (Cst ()) Idt) () -- data Bird o = MkBird o () o


birdDown :: Bird o -> CodeBird o
birdDown (MkBird v () w) = MkPrd (MkIdt v, MkPrd (MkCst (), MkIdt w))

birdUp   :: CodeBird o -> Bird o
birdUp (MkPrd (MkIdt v, MkPrd (MkCst (), MkIdt w))) = MkBird v () w

identity :: Bird o -> Bird o
identity = birdUp . birdDown

birdMap :: (a -> b) -> Bird a -> Bird b
birdMap f = birdUp . fmap f . birdDown


------------------------------------------------------------------------
-- Encodable

class Encodable t where
  type Code t :: Type -> Type
  encode :: t a -> Code t a
  decode :: Code t a -> t a

instance Encodable Bird where
  type Code Bird = CodeBird
  encode = birdDown
  decode = birdUp

gfmap :: (Encodable t, Functor (Code t))
      => (a -> b) -> t a -> t b
gfmap f = decode . fmap f . encode

------------------------------------------------------------------------
-- List

-- data Bird o = MkBird o () o
data List a = Nil | Cons a (List a)

-- newtype Prd k l i = MkPrd { runPrd :: (k i, l i) } deriving Show
newtype Sum k l f i = MkSum { runSum :: Either (k f i) (l f i) }

instance (Functor (k f), Functor (l f)) => Functor (Sum k l f) where
  fmap f (MkSum (Left v)) = MkSum (Left (fmap f v))
  fmap f (MkSum (Right w)) = MkSum (Right (fmap f w))

instance Encodable Maybe where
  type Code Maybe = Sum (Cst ()) Idt ()
  encode Nothing = MkSum (Left (MkCst ()))
  encode (Just x) = MkSum (Right (MkIdt x))

  decode (MkSum (Left (MkCst ()))) = Nothing
  decode (MkSum (Right (MkIdt x))) = Just x

data Fix f a where
  MkFix :: f (Fix f) a -> Fix f a

instance Functor (f (Fix f)) => Functor (Fix f) where
  fmap f (MkFix v) = MkFix (fmap f v)

newtype Rec f a = MkRec { runRec :: f a }
instance Functor f => Functor (Rec f) where
  fmap f (MkRec v) = MkRec (fmap f v)


type Void = Fix Rec ()
absurd :: Void -> Void
absurd (MkFix (MkRec v)) = absurd v

instance Encodable [] where
  type Code [] = Fix (Sum (Cst ()) (Prd Idt Rec))
  encode [] = MkFix (MkSum (Left (MkCst ())))
  encode (x : xs) = MkFix (MkSum (Right (MkPrd (MkIdt x, MkRec (encode xs)))))
  decode (MkFix (MkSum (Left (MkCst ())))) = []
  decode (MkFix (MkSum (Right (MkPrd (MkIdt x, MkRec xs))))) = x : decode xs

------------------------------------------------------------------------
-- Rose Trees

data Rose a = MkRose a [Rose a] deriving Show

newtype Cmp k l f i = MkCmp { runCmp :: k f (l f i) }
instance (Functor (k f), Functor (l f)) => Functor (Cmp k l f) where
  fmap f (MkCmp t) = MkCmp (fmap (fmap f) t)

newtype KCmp t l f i = MkKCmp { runKCmp :: t (l f i) }
instance (Functor t, Functor (l f)) => Functor (KCmp t l f) where
  fmap f (MkKCmp t) = MkKCmp (fmap (fmap f) t)

instance Encodable Rose where
  type Code Rose = Fix (Prd Idt (KCmp [] Rec))
  encode (MkRose x xs) = MkFix (MkPrd (MkIdt x, MkKCmp (MkRec . encode <$> xs)))
  decode (MkFix (MkPrd (MkIdt x, MkKCmp xs))) = MkRose x (decode . runRec <$> xs)

rose :: Rose String
rose = gfmap show (MkRose 1 [MkRose 2 [], MkRose 3 [], MkRose 4 [MkRose 5 []]])
