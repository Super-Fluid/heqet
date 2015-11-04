module List where

import Types

import Control.Lens

-- like groupBy but doesn't preserve list order,
-- instead grouping as much as possible.
-- makeBucketsBy (==) [1,2,3,1,3,4] -> 
makeBucketsBy :: (a -> a -> Bool) -> [a] -> [[a]]
makeBucketsBy comp xs = foldr f [] xs where
    f x [] = [[x]]
    f x (y:ys) = if x `comp` (head y) -- y can never be []
               then (x:y):ys
               else y:(f x ys)