{-# LANGUAGE TypeFamilies #-}

module Utility.Cond where

import Prelude hiding ((||), (&&), not, or, and)

class Cond c where
    type Boolean c
    toBool :: c -> Boolean c
    fromBool :: Boolean c -> c
    -- true, false :: Boolean c -- OK
    
    not     ::  c  -> Boolean c
    
    infixr 2 ||     -- a OR b
    infixr 3 &&     -- a AND b
    infixr 1 %%     -- EITHER a OR b
    
    (&&) :: c -> c -> Boolean c
    (||) :: c -> c -> Boolean c
    (%%) :: c -> c -> Boolean c

instance Cond Bool where
    type Boolean Bool = Bool
    toBool = id
    fromBool = id
    -- true  = True -- OK
    -- false = False -- OK
    
    not x     = if x then False else True
    
    p || q    = if p then True else q
    p && q    = if p then q else False
    p %% q    = if p then not q else q

instance (Cond b) => Cond (a -> b) where
    type Boolean (a -> b) = a -> Boolean b
    toBool f a = toBool $ f a
    fromBool f a = fromBool $ f a
    -- true  a = const (true  :: Boolean b) a -- doesn't like it
    -- false a = undefined -- doesn't like it
    
    not f a    = not $ f a
    
    (f || g) a = f a || g a
    (f && g) a = f a && g a
    (f %% g) a = f a %% g a

and :: Cond c => [c] -> Boolean c
and [c]   = toBool c
and (h:t) = h && fromBool (and t)

or :: Cond c => [c] -> Boolean c
or [c]   = toBool c
or (h:t) = h || fromBool (and t)

infix 4 $==, $/=, ==$, /=$, $==$, $/=$, $<, $>, $<=, $>=, <$, >$, <=$, >=$, $<$, $>$, $<=$, $>=$

($==),  ($/=)  :: Eq b => (a -> b) -> b -> a -> Bool
(==$),   (/=$) :: Eq b => b -> (a -> b) -> a -> Bool
($==$), ($/=$) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool

(f $==  y) x  =  f x == y
(y  ==$ f) x  =  f x == y
(f $==$ g) x  =  f x == g x

(f $/=  y) x  =  f x /= y
(y  /=$ f) x  =  f x /= y
(f $/=$ g) x  =  f x /= g x

($<), ($>), ($<=), ($>=) :: Ord b => (a -> b) -> b -> a -> Bool
(<$), (>$), (<=$), (>=$) :: Ord b => b -> (a -> b) -> a -> Bool
($<$), ($>$), ($<=$), ($>=$) :: Ord b => (a -> b) -> (a -> b) -> a -> Bool


(f $<  y) x  =  f x <  y
(f $<= y) x  =  f x <= y
(f $>  y) x  =  f x >  y
(f $>= y) x  =  f x >= y

(y  <$ f) x   =  y <  f x
(y <=$ f) x   =  y <= f x
(y  >$ f) x   =  y >  f x
(y >=$ f) x   =  y >= f x

(f $<$  g) x  =  f x <  g x
(f $<=$ g) x  =  f x <= g x
(f $>$  g) x  =  f x >  g x
(f $>=$ g) x  =  f x >= g x
