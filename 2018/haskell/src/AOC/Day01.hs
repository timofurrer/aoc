module AOC.Day01 (solve) where

solve :: String -> IO (String, String)
solve input = 
  return (show $ part1 input, 
   show $ part2 input
  )

part1 :: String -> Int
part1 input = sum . parse $ input

parse :: String -> [Int]
parse = map (read . dropWhile (== '+')) . lines

part2 :: String -> Int
part2 input = go [] (cycle . parse $ input)
  where
    go freqs [] = head freqs
    go freqs (x : xs)
      | freq `elem` freqs = go newFreqs []
      | otherwise         = go newFreqs xs
      where
        freq     = x + if length freqs > 0 then head freqs else 0
        newFreqs = freq : freqs
