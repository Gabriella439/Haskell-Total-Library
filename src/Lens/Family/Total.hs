{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

-- | This module lets you exhaustively pattern match on types using
-- `Lens.Family.Lens`es, `Lens.Family.Traversal`s, or `Lens.Family.Prism`s.
--
-- Here's an example use of this library:
--
-- > import Lens.Family.Total
-- > import Lens.Family.Stock
-- >
-- > total :: Either Char Int -> String       -- Same as:
-- > total = _case                            -- total = \case
-- >     & on _Left  (\c -> replicate 3  c )  --     Left  c -> replicate 3 c
-- >     & on _Right (\n -> replicate n '!')  --     Right n -> replicate n '!'
--
-- Our @total@ function pattern matches exhaustively on the `Either` type using
-- the `Lens.Family.Stock._Left` and `Lens.Family.Stock._Right` prisms:
--
-- >>> total (Left 'X')
-- "XXX"
-- >>> total (Right 2)
-- "!!"
--
-- The types ensure that the above function is total.  For example, if you omit
-- the `Lens.Family.Stock._Right` branch:
--
-- > partial :: Either Char Int -> String
-- > partial = _case
-- >     & on _Left  (\c -> replicate 3  c )
--
-- ... then you will get the following type error:
--
-- > No instance for (Empty Int) arising from a use of ‘_case’
-- > In the first argument of ‘(&)’, namely ‘_case’
-- > In the expression: _case & on _Left (\ c -> replicate 3 c)
-- > In an equation for ‘partial’:
-- >     partial = _case & on _Left (\ c -> replicate 3 c)
--
-- That type error means that you didn't pattern match on the branch with the
-- `Int`.
--
-- You can also implement exhaustive pattern matching for your own data types with
-- `Lens.Family.Traversal`s or `Control.Lens.Prism`s.  However, this only works if
-- you have one type variable for each branch of your type:
--
-- > {-# LANGUAGE DeriveGeneric   #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Control.Lens.TH
-- > import GHC.Generics (Generic)
-- > import Lens.Family.Total
-- >
-- > data Example a b c = C1 a | C2 b | C3 c deriving (Generic)
-- >
-- > makePrisms ''Example
-- >
-- > instance (Empty a, Empty b, Empty c) => Empty (Example a b c)
-- >
-- > example :: Example String Char Int -> String  -- Same as:
-- > example = _case                               -- example = \case
-- >     & on _C1 (\s -> s              )          --     C1 s -> s
-- >     & on _C2 (\c -> replicate 3  c )          --     C2 c -> replicate 3  c
-- >     & on _C3 (\n -> replicate n '!')          --     C3 n -> replicate n '!'
--
-- There is no way to prove that the pattern match is exhaustive unless there is
-- a type parameter for every branch.  This is because each successive pattern
-- match `Void`s out that branch's type parameter to prove that the branch no
-- longer needs to be handled.  `_case` just verifies that all type parameters are
-- `Void`.
--
-- You can still write an inexhaustive pattern match so long as you provide a
-- default:
--
-- > example :: Example Int String Float -> String
-- > example = _default "default"
-- >     & on _C2 (\s -> s)
--
-- You can even pattern match using `Lens.Family.Lens`es, too:
--
-- > example :: (Int, Char) -> String     -- Same as:
-- > example = _case                      -- example = \case
-- >     & on _1 (\n -> replicate n '1')  --     (n, _) -> replicate n '1'
--
-- ... and of course the identity lens (`id`) works, too:
--
-- > example :: (Int, Char) -> String        -- Same as:
-- > example = _case                         -- example = \case
-- >     & on id (\(n, c) -> replicate n c)  --     (n, c) -> replicate n c

module Lens.Family.Total (
      Empty(..)
    , _case
    , _default
    , on
    , (&)

    -- * Re-exports
    , Void
    ) where

import Data.Void (Void, absurd)
import Data.Function ((&))
import GHC.Generics

-- | A type class for uninhabited types
class Empty a where
    impossible :: a -> x

    default impossible :: (Generic a, GEmpty (Rep a)) => a -> x
    impossible = gimpossible . from

instance Empty Void where
    impossible = absurd

instance (Empty a, Empty b) => Empty (Either a b) where
    impossible (Left  a) = impossible a
    impossible (Right b) = impossible b

instance Empty a => Empty ((,) a b) where
    impossible (a, _) = impossible a

class GEmpty f where
    gimpossible :: f a -> x

instance GEmpty V1 where
    gimpossible _ = undefined

instance (GEmpty a, GEmpty b) => GEmpty (a :+: b) where
    gimpossible (L1 l) = gimpossible l
    gimpossible (R1 r) = gimpossible r

instance Empty a => GEmpty (K1 i a) where
    gimpossible (K1 k) = impossible k

instance GEmpty a => GEmpty (M1 i c a) where
    gimpossible (M1 m) = gimpossible m

instance GEmpty a => GEmpty (a :*: b) where
    gimpossible (a :*: _) = gimpossible a

-- | Synonym for `impossible`, used to check if a pattern match is exhaustive
_case :: Empty a => a -> x
_case = impossible

-- | Synonym for `const`, used to provide a default if a pattern match is
-- inexhaustive
_default :: x -> a -> x
_default x _ = x

-- | Pattern match on a `Lens.Family.Traversal`
on
    :: ((a -> Either a Void) -> s -> Either a r)
    -> (a -> o)
    -> (r -> o)
    -> s
    -> o
on p f g = either f g . p Left
