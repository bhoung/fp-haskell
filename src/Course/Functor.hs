{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) :: (a -> b) -> f a -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) :: (a -> b) -> ExactlyOne a -> ExactlyOne b
  (<$>) f (ExactlyOne a) = ExactlyOne (f a)


-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) :: (a -> b) -> List a -> List b
  (<$>) f Nil = Nil
  --(<$>) f (x :. xs) = (f x ) :. ((<$>) f xs)
  (<$>) f (x :. xs) = (f x ) :. (f <$> xs)

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) :: (a -> b) -> Optional a -> Optional b
  (<$>) f (Full a) = Full (f a)
  (<$>) f (Empty) = Empty

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) :: (a -> b) -> ((->) t a) -> ((->) t b)
  --(<$>) f g t = f (g t)
  --(<$>) :: (a -> b) -> (t -> a) -> (t -> b)
  (<$>) f g = \t -> f (g t)
--(<$>) = (.)
-- functor for functions is composition
--
-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) :: Functor f => a -> f b -> f a
(<$) a fb = (\_ -> a) <$> fb
-- (\_ -> a) is the same as const a
--(<$) y f = (<$>) (\x -> y) f

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void :: Functor f => f a -> f ()
void fa = (\x ->()) <$> fa
--void f = (<$>) (\x -> ()) f 
--void x = const () <$> x
--
--functor : lifting function into type constructors
--fmap
-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where (<$>) = P.fmap
