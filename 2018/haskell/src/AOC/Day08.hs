module AOC.Day08 (solve) where

import Data.Maybe (fromMaybe)

import Lib.List 

solve :: String -> IO (String, String)
solve input = 
  return (show $ part1 input, 
   show $ part2 input
  )

parse :: String -> [Int]
parse = map read . words

part1 :: String -> Int
part1 input = snd $ nodeMetadataSize $ parse input

nodeMetadataSize :: [Int] -> ([Int], Int)
nodeMetadataSize [] = error "stream wasn't fully consumed"
nodeMetadataSize [_] = error "stream has left over element"
nodeMetadataSize (children : metadataSize : stream) = parseNode children metadataSize stream
  where
    parseNode c m s
      | c == 0    = (drop m s, sum $ take m s)
      | otherwise = let (rs, ms) = nodeMetadataSize s
                        (rs', ms') = parseNode (c - 1) m rs
                     in (rs', ms + ms')

part2 :: String -> Int
part2 input = snd $ nodeValues $ parse input

nodeValues :: [Int] -> ([Int], Int)
nodeValues [] = error "stream wasn't fully consumed"
nodeValues [_] = error "stream has left over element"
nodeValues (children : metadataSize : stream) 
  | children == 0 = (drop metadataSize stream, sum $ take metadataSize stream)
  | otherwise     = let (rs, vs) = foldl (\(rs', vs) _ -> let (rs'', x) = nodeValues rs' in (rs'', vs ++ [x])) (stream, []) [1..children]
                        v = sum $ map (\m -> fromMaybe 0 $ safeIndex vs (m - 1)) $ take metadataSize rs
                     in (drop metadataSize rs, v)
