module AOC.Day07 (solve) where

import qualified Data.Map as MP
import qualified Data.List as L
import Data.Char (ord)

solve :: String -> IO (String, String)
solve input = 
  return (show $ part1 input, 
   show $ part2 input
  )

parse :: String -> [(Char, Char)]
parse = map parseLine . lines

parseLine :: String -> (Char, Char)
parseLine line = (step, previous)
  where
    parts = words line
    step = head $ parts !! 1
    previous = head $ parts !! 7

part1 :: String -> String
part1 input = sortSteps frontier [] $ buildTree reqs MP.empty
  where
    reqs = parse input
    allChars = L.nub $ concat [[a,b] | (a, b) <- reqs]

    frontier :: [Char]
    frontier = L.sort $ [ x | x <- allChars, x `notElem` map snd reqs ]

    buildTree :: [(Char, Char)] -> MP.Map Char [Char] -> MP.Map Char [Char]
    buildTree [] m = m
    buildTree ((a, b) : xs) m = buildTree xs (MP.insertWith (\x y -> L.sort $ x ++ y) a [b] m)

    sortSteps :: [Char] -> [Char] -> MP.Map Char [Char] -> [Char]
    sortSteps [] orderedSteps _ = orderedSteps
    sortSteps (x : xs) orderedSteps tree = sortSteps (L.sort $ xs ++ nextFrontier) (orderedSteps ++ [x]) tree
      where
        nextFrontier = if MP.member x tree
                         then filter (allAvailable orderedSteps tree) (tree MP.! x)
                         else []

        allAvailable :: [Char] -> MP.Map Char [Char] -> Char -> Bool
        allAvailable used m char = length (allUsedReqs `L.intersect` used) == length allUsedReqs - 1
          where
            allUsedReqs = MP.keys $ MP.filter (\v -> char `elem` v) m


maxWorkers :: Int
maxWorkers = 5
secOffset :: Int
secOffset = -65 + 60 

          
part2 :: String -> (Int, [Char])
part2 input = simulateSteps $ buildTree $ parse input
  where
    buildTree :: [(Char, Char)] -> MP.Map Char [Char]
    buildTree xs = buildTree' initTree xs
      where
        initTree = MP.fromList $ map (, []) $ L.nub $ concat $ [[a, b] | (a, b) <- xs ]
        buildTree' :: MP.Map Char [Char] -> [(Char, Char)] -> MP.Map Char [Char]
        buildTree' m [] = m
        buildTree' m ((a, b) : xs') = buildTree' (MP.insertWith (\x y -> L.sort $ x ++ y) b [a] m) xs'

    simulateSteps :: MP.Map Char [Char] -> (Int, [Char])
    simulateSteps = simulateSteps' 0 [] []
      where 
        simulateSteps' :: Int -> [Char] -> [(Char, Int)] -> MP.Map Char [Char] -> (Int, [Char])
        simulateSteps' sec done workers tree 
          | MP.null tree && null workers = (sec - 1, done)
          | otherwise    = let (doneWorkers, remainingWorkers) = L.partition (\(_, t) -> t == 0) workers
                               nowDone = map fst doneWorkers
                               newDone = done ++ nowDone
                               updatedTree = MP.map (L.\\ nowDone) tree

                               updatedWorkers = map (\(c, t) -> (c, t - 1)) remainingWorkers
                               inProgress = map fst updatedWorkers

                               (frontier, remainingTree) = MP.partitionWithKey (\k v -> null v && k `notElem` inProgress) updatedTree

                               idleWorkers = maxWorkers - length updatedWorkers
                               newWorkers = updatedWorkers ++ map (\(c, _) -> (c, ord c + secOffset)) (take idleWorkers $ MP.toAscList frontier)

                               newTree = MP.union remainingTree (MP.fromList $ drop idleWorkers (MP.toAscList frontier))
                           in simulateSteps' (sec + 1) newDone newWorkers newTree
