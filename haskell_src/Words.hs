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
all_same :: [String] -> Bool
equal_words :: String -> String -> Bool

add_word w h =
    let w_upper = Data.List.map toUpper w
    in insertWith (\ [new] old -> new:old) (sort w_upper) [w] h

list_anagrams h =
    let ana_only = Data.HashMap.Strict.filter (\ x -> length x > 1) h
    in elems ana_only

equal_words x y =
    let x_upper = Data.List.map toUpper x
    in x_upper == Data.List.map toUpper y

all_same [] = True
all_same [_] = True
all_same (x:xs) =
    let upper_x = Data.List.map toUpper x
    in all (\ w -> equal_words upper_x (Data.List.map toUpper w)) xs

all_anagrams lst =
    let word_map = Data.List.foldr (add_word) empty lst
    in Data.List.filter (not . all_same) (list_anagrams word_map)
