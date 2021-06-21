{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : stable
Description : Data fulfilling constraints.
-}

module Data.Fulfills
  ( Fulfills(..)
  , Fulfills1(..)
  , (:&:)
  , (:=>)
  ) where

import           Data.Kind (Constraint)
import           Data.Typeable (TypeRep, Typeable, cast, typeOf)

-- | Encapsulate a piece of data with instances to manipulate that data.
data Fulfills c = forall t. c t => Fulfills t

deriving instance (c :=> Show, Show (Fulfills c)) => Show (Fulfills c)

instance (c :=> Eq, c :=> Typeable, Eq (Fulfills c)) => Eq (Fulfills c) where
  Fulfills x == Fulfills y = case cast y of
                                  Just y' -> x == y'
                                  Nothing -> False

instance (c :=> Ord, c :=> Typeable, Ord (Fulfills c), Ord TypeRep) => Ord (Fulfills c) where
  compare (Fulfills x) (Fulfills y) =
    case cast y of
         Just y' -> compare x y'
         Nothing -> compare (typeOf x) (typeOf y)

-- | Encapsulate a container with its instances, but still keep track of its type argument.
data Fulfills1 c a = forall f. c f => Fulfills1 (f a)

instance (c :=> Functor) => Functor (Fulfills1 c) where
  fmap f (Fulfills1 x) = Fulfills1 $ f <$> x
  x <$ (Fulfills1 y) = Fulfills1 $ x <$ y

infixr 5 :&:
-- | Products of constraints.
--
-- In normal situations @(Read :&: Show) a@ = @(Read a, Show a)@, but when you
-- need to pass @k -> Constraint@ to a data type @(Read a, Show a)@ doesn't work.
class (c a, d a) => (:&:) (c :: k -> Constraint) (d :: k -> Constraint) (a :: k)
instance (c a, d a) => (:&:) c d a

infixr 1 :=>
-- | Implication of constraints.
--
-- @Read :&: Show :=> Show@
class (forall a. c a => d a) => (:=>) (c :: k -> Constraint) (d :: k -> Constraint)
instance (forall a. c a => d a) => (:=>) c d
