module Utils
(
        getRight, lookupe, fst3, snd3, trd3, set_equal
) where

import Data.List((\\))
import qualified Data.List as L

getRight :: Either a b -> b
getRight (Right x) = x
getRight _ = error "getRight: unexpected Left"

lookupe :: (Eq a) => a -> [(a,b)] -> b
lookupe x xs = case (lookup x xs) of
        Just y  -> y
        Nothing -> error "lookupe fail."

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

set_equal :: (Eq a) => [a] -> [a] -> Bool
set_equal xs ys = (xs' \\ ys' == []) && (ys' \\ xs' == [])
        where   xs' = L.nub xs
                ys' = L.nub ys
