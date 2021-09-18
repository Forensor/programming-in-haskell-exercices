module Ch7 where

import Data.Char

-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using map
--    and filter.
mNf :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mNf f p = map f . filter p

-- 2. Define the following higher-order functions on list:
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- 3. Redefine map f and filter p using foldr.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x:xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal
--    into an integer.
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x*10 + y) 0

-- 5. Define the higher-order functions curry and uncurry without looking at the library
--    definitions.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x,y) = f x y

-- 6. Redefine the functions chop8, map f and iterate f using unfold.
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map2' :: (a -> b) -> [a] -> [b]
map2' f = unfold null (f . head) tail

iterate2' :: (a -> a) -> a -> [a]
iterate2' = unfold (const False) id

-- 7. Modify the binary string transmitter example to detect simple transmission errors
--    using the concept of parity bits.
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = concatMap (checkParity . setParity) . chop8

oddOnes :: [Bit] -> Bool
oddOnes = odd . length . filter (==1)

setParity :: [Bit] -> [Bit]
setParity bits | oddOnes bits = bits ++ [1]
               | otherwise    = bits ++ [0]

checkParity :: [Bit] -> [Bit]
checkParity bits | parity    = init bits
                 | otherwise = error "Transmission error: 0-parity detected"
                 where parity = last bits == 1

-- 8. Test your new string transmitter using a faulty communication channel that forgets
--    the first bit, which can be modified using the tail function.
faultyChannel :: p -> [Bit] -> [Bit]
faultyChannel bits = channel . tail

-- 9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately
--    applies uts two argument functions to successive elements in a list.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 = zipWith ($) fs
               where fs = cycle [f1,f2]

-- 0. Using altMap, define a function luhn :: [Int] -> Bool that implements the luhn
--    algorithm from the chapter 4 for bank card numbers of any length.
luhnDouble :: Int -> Int
luhnDouble n | double > 9 = double - 9
             | otherwise  = double
             where
                 double = n*2

luhn :: [Int] -> Bool
luhn = (==0) 
     . (`mod` 10) 
     . sum 
     . altMap luhnDouble id