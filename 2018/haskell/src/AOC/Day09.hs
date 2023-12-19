module AOC.Day09 (solve) where

import Debug.Trace

import qualified Data.Map as M

import qualified Data.CircularList as C

import Data.Maybe 

debug = flip trace

solve :: String -> IO (String, String)
solve input = 
  return (show $ part1 input, 
   show $ part2 input
  )

parse :: String -> (Int, Int)
parse input = let w = words . head $ lines input in (read $ head w, read $ w !! 6)

part1 :: String -> Int
part1 input = play [0] (M.fromList $ map (, 0) [1..amountPlayers]) 0 1
  where
    game = parse input
    
    amountPlayers = fst game
    lastMarble = snd game
    

    play :: [Int] -> M.Map Int Int -> Int -> Int -> Int
    play marbleCircle players pos currentMarble
      | currentMarble == lastMarble = maximum players
      | otherwise = play newMarbles newPlayers newPos (currentMarble + 1)
        where 
          ((newMarbles, newPos), newPlayers) = takeTurn marbleCircle players pos currentMarble

    takeTurn marbleCircle players pos currentMarble
      | currentMarble `mod` 23 /= 0 = (insertInCircle marbleCircle pos currentMarble, players)
      | otherwise                   = ((newMarbles, newPos), M.adjust (removed + currentMarble +) (currentMarble `mod` M.size players) players)
        where
          (newMarbles, newPos, removed) = removeFromCircle marbleCircle pos

    insertInCircle :: [Int] -> Int -> Int -> ([Int], Int)
    insertInCircle c i x 
      | i + 2 <= length c = (insertAt x (i + 2) c, i + 2)
      | otherwise         = (insertAt x 1 c, 1)

    removeFromCircle :: [Int] -> Int -> ([Int], Int, Int)
    removeFromCircle c i = (take p c ++ drop (p + 1) c, p, c !! p)
      where 
        p = if i - 7 >= 0 
              then i - 7
              else i - 7 + length c


part2 :: String -> Int
part2 input = play (C.fromList [0]) (M.fromList $ map (, 0) [1..amountPlayers]) 1
  where
    game = parse input
    
    amountPlayers = fst game
    lastMarble = snd game * 100 

    play :: C.CList Int -> M.Map Int Int -> Int -> Int
    play marbleCircle players currentMarble
      | currentMarble == lastMarble = maximum players
      | otherwise                   = play newCircle newPlayers (currentMarble + 1)
        where
          (newCircle, newPlayers) = takeTurn marbleCircle players currentMarble

          takeTurn :: C.CList Int -> M.Map Int Int -> Int -> (C.CList Int, M.Map Int Int)
          takeTurn m p c 
            | c `mod` 23 == 0 = let m' = C.rotN 7 m 
                                    p' = M.adjust (c + fromMaybe 0 (C.focus m') +) (c `mod` M.size p) p
                                    m'' = C.removeL m'
                                 in (m'', p')
            | otherwise       = (C.insertR c $ C.rotL m, p)


insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as
