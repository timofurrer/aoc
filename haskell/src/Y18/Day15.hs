module Y18.Day15 (solve) where

-- Imports

import Lib.Grid
import Lib.Point
import Lib.Paths

import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.Map.Strict as SMap

-- Input Parsing 

type Input = (Grid Char, [Unit])

data UnitType = Elf | Goblin deriving (Show, Eq)

data Unit = Unit
  { idx :: !Int
  , hp :: !Int    -- Health Points
  , ap :: !Int    -- Attack Power
  , pos :: !Point -- Current position
  , typ :: !UnitType 
  }
  deriving (Show, Eq)

instance Ord Unit where
  u1 `compare` u2 = pos u1 `compare` pos u2

parse :: String -> Input
parse input = let g = parseFromString input in (g, parseUnits g)

parseUnits :: Grid Char -> [Unit]
parseUnits = L.sort . zipWith (curry newUnit) [0..] . L.sortBy (compare `F.on` fst) . filterUnits . allPointsWithValue
  where
    filterUnits = filter (\(_, c) -> c == 'E' || c == 'G')

    newUnit :: (Int, (Point, Char)) -> Unit
    newUnit (i, (p, 'E')) = Unit {idx=i, hp=200, ap=3, pos=p, typ=Elf}
    newUnit (i, (p, 'G')) = Unit {idx=i, hp=200, ap=3, pos=p, typ=Goblin}
    newUnit (_, (p, c))   = error ("invalid unit type " ++ show c ++ " at " ++ show p)

-- Common

data PlayType = Attack | Move deriving Show

enemyType :: UnitType -> UnitType
enemyType Elf = Goblin
enemyType Goblin = Elf

enemies :: Unit -> [Unit] -> [Unit]
enemies (Unit _ _ _ _ t) = filter (\u@(Unit _ _ _ _ et) -> isAlive u && et == enemyType t)

elves :: Unit -> Bool
elves = (==) Elf . typ

hasNoEnemies :: Unit -> [Unit] -> Bool
hasNoEnemies u = null . enemies u

isAlive :: Unit -> Bool
isAlive u = hp u > 0

isDead :: Unit -> Bool
isDead = not . isAlive

unitFromIdx :: Int -> [Unit] -> Unit
unitFromIdx _ []       = error "unit not found"
unitFromIdx i (u : us) 
  | idx u == i = u
  | otherwise  = unitFromIdx i us

targetInRangeToAttack :: Unit -> [Unit] -> Maybe Unit
targetInRangeToAttack u us = 
  let ns = fourNeighborPoints $ pos u 
      targets = filter (\u' -> pos u' `elem` ns) $ enemies u us
      nearestTargets = head . L.groupBy (\u1 u2 -> hp u1 == hp u2) $ L.sortBy fewestHitPoints targets
   in if null targets 
        then Nothing
        else Just (minimum nearestTargets)

possibleMoves :: Grid Char -> [Unit] -> Unit -> Point -> [Point]
possibleMoves g us u p = 
  let notObstacle p' = g ! p' /= '#'
      notUnit p' = p' `notElem` (map pos . filter isAlive $ filter (\u' -> typ u' == typ u) us)
   in if p `elem` (map pos . filter isAlive $ filter (\u' -> typ u == enemyType (typ u')) us)
        then []
        else filter notUnit . filter notObstacle $ fourNeighborPoints p

nextPossibleMove :: Grid Char -> [Unit] -> Unit -> Maybe Point
nextPossibleMove g us u
  | null minTargets = Nothing
  | otherwise       = Just nextMove
  where
    bfsTargets = bfs (pos u) (possibleMoves g us u)
    targets = map pos $ enemies u us
    minTargets = minimumTargets targets bfsTargets
    minTarget = minimum $ concatMap (\t -> snd $ bfsTargets SMap.! t) minTargets
    shortestPathsToTargets = reconstructPaths bfsTargets minTarget
    nextMove = minimum $ map (head . tail) shortestPathsToTargets

roundUnit :: Grid Char -> [Unit] -> Unit -> Maybe [Unit]
roundUnit g us u
  | isDead u                             = Just us
  | hasNoEnemies u us                    = Nothing
  | Just t <- targetInRangeToAttack u us = Just $ attack u t us
  | Just p <- nextPossibleMove g us u    = 
    let us' = move u p us
        u' = unitFromIdx (idx u) us'
     in case targetInRangeToAttack u' us' of
          Nothing -> Just us'
          Just t  -> Just $ attack u' t us'
  | otherwise                            = Just us

attack :: Unit -> Unit -> [Unit] -> [Unit]
attack _ _ [] = []
attack u t (u' : us) 
  | t == u'   = t {hp=hp t - ap u} : us
  | otherwise = u' : attack u t us

move :: Unit -> Point -> [Unit] -> [Unit]
move _ _ [] = []
move u p (u' : us)
  | u == u'   = u' {pos=p} : us
  | otherwise = u' : move u p us

type HitPointSum = Int

hitPointSum :: [Unit] -> HitPointSum
hitPointSum = sum . map hp . filter isAlive

fewestHitPoints :: Unit -> Unit -> Ordering
fewestHitPoints u1 u2 = hp u1 `compare` hp u2

playRound :: Grid Char -> [Unit] -> Either [Unit] (HitPointSum, [Unit])
playRound g us = go (map idx $ L.sort $ filter isAlive us) us
  where
    go :: [Int] -> [Unit] -> Either [Unit] (HitPointSum, [Unit])
    go [] us'         = Left us'
    go (un : uns) us' = 
      let u = unitFromIdx un us'
       in case roundUnit g us' u of
            Nothing   -> Right (hitPointSum us', us')
            Just us'' -> go uns us''

-- Solutions

--- Part 1

play1 :: Int -> Grid Char -> [Unit] -> Int
play1 r g us = 
  case playRound g us of
    Left us'  -> play1 (r + 1) g us'
    Right (hps, _) -> (r - 1) * hps

part1 :: Input -> Int
part1 = uncurry (play1 1)

--- Part 2

play2 :: Int -> Grid Char -> [Unit] -> (Bool, Int)
play2 r g us = 
  case playRound g us of
    Left us'  -> play2 (r + 1) g us'
    Right (hps, us') -> (allElvesAlive us', (r - 1) * hps)
  where 
    allElvesAlive = all isAlive . filter elves
          
part2 :: Input -> Int
part2 = uncurry (go (4 :: Int))
  where
    go attackPower g us =
      case play2 1 g (fixAp attackPower us) of
        (False, _) -> go (attackPower + 1) g us
        (True, hps) -> hps
    fixAp x = map (fix x)
      where
        fix _ u@(Unit _ _ _ _ Goblin) = u
        fix ap' u = u { ap=ap' }

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parse rawInput 
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
