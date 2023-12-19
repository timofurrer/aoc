module AOC.Day02 (solve) where

import qualified Data.Map as MP

solve :: String -> IO (String, String)
solve input = do
  return (show $ part1 input, show $ part2 input)

parse :: String -> [String]
parse = lines

part1 :: String -> Int
part1 input = checksum . map (\s -> counter s MP.empty) $ parse input
  where
    counter :: String -> MP.Map Char Int -> (Int, Int)
    counter [] m = hasTwosAndThrees m
    counter (x : xs) m = counter xs (MP.insertWith (+) x 1 m)

    hasTwosAndThrees m = (hasTwos m, hasThrees m)

    countOcc n = length . MP.filter (== n)

    hasTwos m = min 1 $ countOcc 2 m
    hasThrees m = min 1 $ countOcc 3 m

    checksum :: [(Int, Int)] -> Int
    checksum xs = 
      uncurry (*) $ 
        foldr1 (\acc c -> (fst acc + fst c, snd acc + snd c)) xs

part2 :: String -> String
part2 input = findSimilar $ combinations $ parse input
  where
    findSimilar [] = error "no similar box ids found"
    findSimilar ((xa, xb) : xs) = 
      if isSimilar 
        then map fst $ equalChars xa xb
        else findSimilar xs
      where
        isSimilar = length xa - 1 == (length $ filter (\(x, y) -> x == y) $ zip xa xb)
        equalChars a b = filter (uncurry (==)) $ zip a b

combinations :: [String] -> [(String, String)]
combinations xs = go xs [] xs
  where
    go :: [String] -> [(String, String)] -> [String] -> [(String, String)]
    go [] combs _ = combs
    go (x : xs) combs xs' = go xs (combs ++ combinations' xs') xs'
      where
        combinations' [] = []
        combinations' (x' : xs') = (x, x') : combinations' xs'
