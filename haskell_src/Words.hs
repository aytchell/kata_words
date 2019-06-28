module Words
( anagrams
, palindrome
) where

import Data.List

-- Signatures of exported functions

palindrome :: (Ord a) => [a] -> Bool
anagrams :: (Ord a) => [a] -> [a] -> Bool

-- Implementations of functions

palindrome x = x == reverse x

anagrams x y = sort x == sort y
