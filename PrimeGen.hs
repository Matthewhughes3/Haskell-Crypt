module PrimeGen where

import System.Random
import System.Environment

main = do
  gen <- getStdGen
  (bits:_) <- getArgs
  let r = getPrime (read bits) gen
  putStrLn $ show r

getPrime :: (RandomGen g) => Integer -> g -> Integer
getPrime bits gen = let (r,_) = head $ dropWhile (\(num,bool) -> not bool) $ map (\x -> (x, fermat x gen)) (randomBits bits gen)
                    in r

decToBits :: Integer -> [Bool]
decToBits n = map odd powers
                where powers = takeWhile (>0) $ map (\x -> n `div` 2^x) [0..]

-- Right-to-left binary modular exponentiation
-- This is necessary for very large numbers
modExp :: Integer -> Integer -> Integer -> Integer
modExp a b m = let bits = decToBits b
               in  bitModExp a bits 1 m
               where bitModExp a [] r m = r
                     bitModExp a (b:bs) r m = if b 
                                                then bitModExp (a^2 `mod` m) bs (r * a `mod` m) m 
                                                else bitModExp (a^2 `mod` m) bs r m

randomBits :: (RandomGen g) => Integer -> g -> [Integer]
randomBits b g = randomRs (2^(b - 1), 2^b - 1) g

-- TODO: Add miller-rabin implementation to further ensure the number is prime
fermat :: (RandomGen g) => Integer -> g -> Bool
fermat p g 
  | even p = False
  | otherwise = modExp a (p - 1) p == 1 && modExp a' (p - 1) p == 1
                where [a, a'] = take 2 $ randomRs (2, (p - 2)) g
