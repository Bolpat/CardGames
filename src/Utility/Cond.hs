{-# LANGUAGE TypeFamilies, RebindableSyntax #-}

{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Utility.Cond
    (
        --module Prelude hiding ((||), (&&), not, or, and)),
        Cond, and, or, (%%), (&&), (||), true, false,
        ($==), ($/=), (==$), (/=$), ($==$), ($/=$), ($<), ($>), ($<=), ($>=), (<$), (>$), (<=$), (>=$), ($<$), ($>$), ($<=$), ($>=$)
    )
where

import Prelude hiding ((||), (&&), not, or, and)

class Cond c where
    type Boolean c
    --toBool :: c -> Boolean c
    fromBool :: Boolean c -> c
    true  :: c
    false :: c
    
    not ::  c  -> Boolean c
    
    infixr 2 ||     -- a OR b
    infixr 3 &&     -- a AND b
    infixr 1 %%     -- EITHER a OR b
    
    (&&) :: c -> c -> Boolean c
    (||) :: c -> c -> Boolean c
    (%%) :: c -> c -> Boolean c

instance Cond Bool where
    type Boolean Bool = Bool

    --toBool   = id
    fromBool = id
    true  = True
    false = False
    
    not True  = False
    not False = True

    False || False = False
    _     || _     = True
    
    True  && True  = True
    _     && _     = False
    
    True  %% False = True
    False %% True  = True
    _     %% _     = False

instance Cond b => Cond (a -> b) where
    type Boolean (a -> b) = a -> Boolean b

    --toBool   f a = toBool   $ f a
    fromBool f a = fromBool $ f a
    true  = const true -- const (true :: b) a -- doesn't like it
    false = const false -- doesn't like it
    
    not f a    = not $ f a
    
    (f || g) a = f a || g a
    (f && g) a = f a && g a
    (f %% g) a = f a %% g a

and :: Cond c => [c] -> c
and []    = true
and (h:t) = fromBool $ h && and t

or :: (Cond c) => [c] -> c
or []    = false
or (h:t) = fromBool $ h || or t

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