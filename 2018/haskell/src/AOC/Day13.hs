module AOC.Day13 (solve) where

-- Imports

import Lib.Debug

import Lib.List
import Lib.Grid
import Lib.Point

import qualified Data.Maybe as M
import qualified Data.List as L

-- Input Parsing 

type Input = (Grid Char, [Cart])

type Cart = (Point, Direction, [TurnF])

type TurnF = Point -> Direction -> (Point, Direction)

turnCycle :: [TurnF]
turnCycle = cycle [turnL, straight, turnR]

straight :: Point -> Direction -> (Point, Direction)
straight p d = (p --> d, d)

turnL :: Point -> Direction -> (Point, Direction)
turnL = turn turnCounterClockwise90

turnR :: Point -> Direction -> (Point, Direction)
turnR = turn turnClockwise90 

turn :: (Direction -> Direction) -> Point -> Direction -> (Point, Direction)
turn f p d = let nd = f d 
              in (p --> nd, nd)

parse :: String -> Input
parse string = let grid = parseFromString string
                   carts = findCarts grid
                in (grid // map convertCartToTrack carts, carts)
  where
    findCarts :: Grid Char -> [Cart]
    findCarts g = eCarts ++ wCarts ++ nCarts ++ sCarts
      where
        eCarts = map (\(p, _) -> (p, E, turnCycle)) $ filter (\(_, c) -> c == '>') $ allPointsWithValue g
        wCarts = map (\(p, _) -> (p, W, turnCycle)) $ filter (\(_, c) -> c == '<') $ allPointsWithValue g
        nCarts = map (\(p, _) -> (p, N, turnCycle)) $ filter (\(_, c) -> c == '^') $ allPointsWithValue g
        sCarts = map (\(p, _) -> (p, S, turnCycle)) $ filter (\(_, c) -> c == 'v') $ allPointsWithValue g

    convertCartToTrack :: Cart -> (Point, Char)
    convertCartToTrack (p, N, _) = (p, '|')
    convertCartToTrack (p, E, _) = (p, '-')
    convertCartToTrack (p, S, _) = (p, '|')
    convertCartToTrack (p, W, _) = (p, '-')

t3fst (a, _, _) = a

-- Part 1

part1 :: Input -> Point
part1 (grid, carts) = driveUntilCollision grid carts

driveUntilCollision :: Grid Char -> [Cart] -> Point
driveUntilCollision g cs = let nc = driveCarts g (L.sortOn t3fst cs) 
                            in case nc of 
                                Left cs' -> driveUntilCollision g cs'
                                Right p -> p

getCrash :: [Point] -> Maybe Point
getCrash points = go $ zip [0..] points
  where
    go []            = Nothing
    go ((i, p) : ps) = if any (\(i', p') -> p == p' && i /= i') $ zip [0..] points
                         then Just p
                         else go ps

driveCarts :: Grid Char -> [Cart] -> Either [Cart] Point
driveCarts _ [] = Left []
driveCarts g (c : cs) = let dc = driveCart g c 
                            crash = getCrash $ map t3fst (dc : cs)
                            dcs = driveCarts g cs
                         in case crash of 
                              Just p -> Right p 
                              Nothing -> case dcs of 
                                Left cs' -> Left (dc : cs')
                                Right p -> Right p
                          
driveCart :: Grid Char -> Cart -> Cart
driveCart g (p, d, t)
  | g ! p == '\\' && d == E = (p --> S, S, t)
  | g ! p == '\\' && d == W = (p --> N, N, t)
  | g ! p == '\\' && d == N = (p --> W, W, t)
  | g ! p == '\\' && d == S = (p --> E, E, t)
  | g ! p == '/' && d == E = (p --> N, N, t)
  | g ! p == '/' && d == W = (p --> S, S, t)
  | g ! p == '/' && d == N = (p --> E, E, t)
  | g ! p == '/' && d == S = (p --> W, W, t)
  | g ! p == '-' && d == E = (p --> d, d, t)
  | g ! p == '-' && d == W = (p --> d, d, t)
  | g ! p == '|' && d == N = (p --> d, d, t)
  | g ! p == '|' && d == S = (p --> d, d, t)
  | g ! p == '+' = let (tf, nt) = pop t 
                       (np, nd) = tf p d
                    in (np, nd, nt)
  | otherwise = error ("invalid combination of cart position and direction at " ++ show p ++ " with direction " ++ show d)
        

-- Part 2
          
part2 :: Input -> Point
part2 (grid, carts) = driveUntilLast grid carts

driveUntilLast :: Grid Char -> [Cart] -> Point
driveUntilLast g cs 
  | length cs == 1 = t3fst $ head cs
  | otherwise      = let nc = driveCartsAndRemove g (L.sortOn t3fst cs) []
                      in driveUntilLast g nc

driveCartsAndRemove :: Grid Char -> [Cart] -> [Cart] -> [Cart]
driveCartsAndRemove _ [] cs' = cs'
driveCartsAndRemove g (c : cs) cs' = let dc = driveCart g c 
                                      in if t3fst c `elem` map t3fst cs' 
                                          then driveCartsAndRemove g cs (L.deleteBy (\x y -> t3fst x == t3fst y) c cs')
                                          else if t3fst dc `elem` map t3fst cs'
                                                then driveCartsAndRemove g cs (L.deleteBy (\x y -> t3fst x == t3fst y) dc cs')
                                                else driveCartsAndRemove g cs (dc : cs')

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parse rawInput 
  putStrLn "Parsed Input:"
  putStr $ display $ fst input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
