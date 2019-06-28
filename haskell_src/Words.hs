module Words
( anagrams
, palindrome
, all_anagrams
) where

import Data.List
import Data.HashMap.Strict -- you need the 'unordered-containers' package
import Data.Char

-- Signatures of exported functions

palindrome :: [String] -> Bool
anagrams :: [String] -> [String] -> Bool
all_anagrams :: [String] -> [[String]]

-- Implementations of functions 'palindrome' and 'anagrams'

palindrome x = x == reverse x

anagrams x y = sort x == sort y

-- Implementation of 'all_anagrams' with helpers

add_word:: String -> HashMap String [String] -> HashMap String [String]
list_anagrams:: HashMap String [String] -> [[String]]

add_word w h =
    let w_upper = Data.List.map toUpper w
    in insertWith (\ [new] old -> new:old) (sort w_upper) [w] h

list_anagrams h =
    let ana_only = Data.HashMap.Strict.filter (\ x -> length x > 1) h
    in elems ana_only

all_anagrams lst =
    let word_map = Data.List.foldr (add_word) empty lst
    in list_anagrams word_map
