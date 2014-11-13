module Useful
( map2
, mmap
, orElse
, try
, fix
, pick 
) where

-- Maps each value in a two tuple to two new values
-- with two respective functions
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Applies a function to a Maybe value,
-- if it is not Nothing 
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes in two Maybe values and returns
-- the first if it is something, second otherwise
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
    
-- takes in a function that potentially returns Maybe
-- and applies it to an x, if it's Nothing, then it 
-- returns x, otherwise it returns f x 
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- finds the fixed point, or the point where f(x) == x
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- picks the element r way through the list
-- 1/2 -> picks the element 1/2 way through the list
-- 1/3 -> ""    ""  ""      1/3
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
