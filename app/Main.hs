module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
    putStrLn "1. average"
    print (average [1..10])
    print (average [1..100])
    print (average [1..1000])
    putStrLn ""
    putStrLn "2. divides, divides2, isPrime"
    print (divides 100)
    print (divides2 100)
    print (isPrime 11)
    putStrLn ""
    putStrLn "3. prefix, substring"
    print (prefix "abc" "abcd")
    print (prefix "abc" "bcd")
    print (substring "abc" "dabcd")
    print (substring "abc" "d")
    putStrLn ""
    putStrLn "4. permut"
    print (permut [1,2,3] [3,2,1])
    print (permut [1,2,3] [3,2])
    print (permut [1,2,3] [3,2,1,1])
    print (permut2 [1,2,3] [3,2,1])
    print (permut2 [1,2,3] [3,2])
    print (permut2 [1,2,3] [3,2,1,1])
    putStrLn ""
    putStrLn "5. capitalise"
    print (capitalise "abc123abc")
    print (capitalise "abc123abcAAAAAAA")
    print (capitalise "abc123abc!@#@#%#$%^")
    putStrLn ""
    putStrLn "6. itemTotal, itemDiscount"
    print (itemTotal [("a", 1), ("a", 2), ("b", 3), ("a", 3), ("b", 4)])
    print (itemDiscount "a" 90 [("a", 1), ("a", 2), ("b", 3), ("a", 3), ("b", 4)])
    putStrLn ""
    

--1
average :: [Float] -> Float
average x = sum x / fromIntegral (length x)

--2
divides, divides2 :: Integer -> [Integer]
divides x
    | x <= 0 = error "argument must be larger than 0!"
    | otherwise = div' x x
    where
        div' x 0 = []
        div' x y = if mod x y == 0 then y : div' x (y-1) else div' x (y-1)

divides2 x
    | x <= 0 = error "argument must be larger than 0!"
    | otherwise = [y | y <- [1..x], mod x y == 0]

isPrime :: Integer -> Bool
isPrime x = length (divides2 x) == 2

--3
prefix, substring :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring x (y:ys) = prefix x (y:ys) || prefix x ys

--4
permut, permut2 :: [Integer] -> [Integer] -> Bool
permut x y = sort x == sort y

permut2 [] [] = True
permut2 (x:xs) (y:ys)
    | length xs /= length ys = False
    | x == y = permut2 xs ys
    | otherwise = permut2 xs (y:(remove1 x ys))
    where
        remove1 _ [] = []
        remove1 x (y:ys)
            | x == y = ys
            | otherwise = y:(remove1 x ys)

--5
capitalise :: String -> String
capitalise x = [toUpper y | y <- x, elem y (['a'..'z'] ++ ['A'..'Z'])]

--6
itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal [] = []
itemTotal ((x, xf):ys) = (x, xf + sum [zf | (z, zf) <- ys, z == x]) : itemTotal [(z, zf) | (z, zf) <- ys, z /= x]

itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscount _ _ [] = []
itemDiscount x y z = [(zs, zf * fromIntegral (100 - y) / 100.0) | (zs, zf) <- z, zs == x] ++ [(zs, zf) | (zs, zf) <- z, zs /= x]