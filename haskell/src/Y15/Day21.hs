module Y15.Day21 where

-- Imports

import Lib.Debug
import Lib.Parsing
import Data.Maybe (mapMaybe)

-- Input Parsing 

type Input = Player

type Player = (Int, Int, Int) -- (hp, damage, armor)

parseInput :: String -> Input
parseInput = parseWith boss
  where
    boss = do
      _ <- symbol "Hit Points:"
      hp <- number
      _ <- symbol "Damage:"
      da <- number
      _ <- symbol "Armor:"
      ar <- number
      pure (hp, da, ar)

type Item = (Int, Int, Int)
type Weapon = Item
type Armor = Item
type Ring = Item

weapons :: [Weapon]
weapons = [
  (8, 4, 0),  -- Dagger
  (10, 5, 0), -- Shortsword
  (25, 6, 0), -- Warhammer
  (40, 7, 0), -- Longsword
  (74, 8, 0)] -- Greataxe

armors :: [Armor]
armors = [
  (0, 0, 0),
  (13, 0, 1),  -- Leather
  (31, 0 ,2),  -- Chainmail
  (53, 0, 3),  -- Splintmail
  (75, 0, 4),  -- Bandedmail
  (102, 0, 5)] -- Platemail

rings :: [Ring]
rings = [
  (0, 0, 0),
  (25, 1, 0),
  (50, 2, 0),
  (100, 3, 0),
  (20, 0, 1),
  (40, 0, 2),
  (80, 0, 3)]

-- Common

shop :: [Item]
shop = weapons ++ armors ++ rings

settings :: [[Item]]
settings = filter dupRings $ sequence [weapons, armors, rings, rings]
  where
    dupRings [_, _, r1, r2] = r1 /= r2
    dupRings _ = error "invalid setting"

totals :: [Item] -> (Int, Int, Int)
totals = foldl1 (\(c, d, a) (cd, dd, ad) -> (c + cd, d + dd, a + ad))

me :: [Item] -> (Int, Player)
me is = let (c, d, a) = totals is in (c, (100, d, a))

damage :: Player -> Player -> Int
damage (_, d, _) (_, _, a) = max 1 (d - a)

hp :: Player -> Int
hp (h, _, _) = h

battle :: Player -> Player -> Bool
battle p1 p2 = hp p2 `div` damage p1 p2 < hp p1 `div` damage p2 p1

-- Solutions

--- Part 1

part1 :: Input -> Int
part1 boss = minimum $ mapMaybe battleCost settings
  where
    battleCost :: [Item] -> Maybe Int
    battleCost is = let (cost, m) = me is in if battle m boss then Just cost else Nothing

--- Part 2

part2 :: Input -> Int
part2 boss = maximum $ mapMaybe battleCost settings
  where
    battleCost :: [Item] -> Maybe Int
    battleCost is = let (cost, m) = me is in if battle m boss then Nothing else Just cost

-- Main

solve :: String -> IO (String, String)
solve rawInput = do
  let input = parseInput rawInput
  putStrLn "Parsed Input:"
  print input
  putStrLn ""
  return (show $ part1 input, show $ part2 input)
