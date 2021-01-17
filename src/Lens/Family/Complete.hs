{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module Lens.Family.Complete
    ( Full(..)
    , trivialVal
    , GFull(..)
    , _cocase
    , at

    -- * Re-exports
    , (&)
    , (&&&)
    ) where

import Data.Functor.Identity
import Data.Function ((&))
import Control.Arrow ((&&&))
import GHC.Generics

-- A typeclass for trivially inhabited types
class Full a where
    trivial :: x -> a

    default trivial :: (Generic a, GFull (Rep a)) => x -> a
    trivial = to . gtrivial

-- | A version of 'trivial' without the arrow.
trivialVal :: Full a => a
trivialVal = trivial ()

instance Full () where
    trivial = const ()

instance (Full a, Full b) => Full (a, b) where
    trivial = trivial &&& trivial

instance Full a => Full (Either a b) where
    trivial = Left . trivial

class GFull f where
    gtrivial :: x -> f a

instance GFull U1 where
    gtrivial = const U1

instance (GFull a, GFull b) => GFull (a :*: b) where
    gtrivial x = gtrivial x :*: gtrivial x

instance Full a => GFull (K1 i a) where
    gtrivial = K1 . trivial

instance GFull a => GFull (M1 i c a) where
    gtrivial = M1 . gtrivial

instance GFull a => GFull (a :+: b) where
    gtrivial = L1 . gtrivial

-- | Synonym for `trivial`, used to check if a copattern is complete
_cocase :: Full a => x -> a
_cocase = trivial

-- | Copattern match on a `Lens.Family.Traversal`.
-- We use a general @'Full' u@ as the target of the traversal
-- to allow more flexibility for nested products. Given
--
-- @
-- data Rec1 a b = Rec1 { _foo :: a, _bar :: b }
-- data Rec2 a b c = Rec2 { _baz :: Rec1 a b, _quux :: c }
-- @
--
-- we can build a @Rec2 a b c@ from a @Rec2 () () ()@ using traversals
-- @baz . foo@ and @baz . bar@ to target @a@ and @b@ (each of which will
-- start out as @()@), or using @baz@ to target @Rec1 a b@ (which will start
-- out as @Rec1 () ()@).
at
    :: Full u
    => ((u -> Identity b) -> s -> Identity t)
    -> (i -> b)
    -> (i -> s)
    -> i
    -> t
at p f g = simpleAt p <$> f <*> g

-- | A version of 'at' that does not build in @Reader@.
simpleAt
    :: Full u
    => ((u -> Identity b) -> s -> Identity t)
    -> b
    -> s
    -> t
simpleAt p b s = runIdentity $ p (const $ Identity $ b) s
