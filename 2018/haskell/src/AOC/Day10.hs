module AOC.Day10 (solve) where

import Lib.Parsing

-- Main

solve :: String -> (String, String)
solve input = 
  (part1 $ parseInput input, 
   show $ part2 $ parseInput input
  )

-- Input Parsing 

type Input = [(Int, Int, Int, Int)]

parseInput :: String -> Input
parseInput = parseLinesWith points
  where
    points = do
      _ <- symbol "position=<"
      x <- number
      _ <- symbol ","
      y <- number
      _ <- symbol "> velocity=<"
      dx <- number
      _ <- symbol ","
      dy <- number
      pure (x, y, dx, dy)

draw :: Input -> String
draw input = drawRow minY
  where
    minX = minimum $ map (\(x, _, _, _) -> x) input
    maxX = maximum $ map (\(x, _, _, _) -> x) input
    minY = minimum $ map (\(_, y, _, _) -> y) input
    maxY = maximum $ map (\(_, y, _, _) -> y) input

    drawRow y
      | y == maxY + 1 = "\n"
      | otherwise     = drawCol y minX ++ drawRow (y + 1)

    drawCol y x
      | x == maxX + 1 = "\n"
      | otherwise     = getPoint x y ++ drawCol y (x + 1)

    getPoint x y = if null point
                    then "."
                    else "#"
      where
        point = filter (\(x', y', _, _) -> x == x' && y == y') input


-- Part 1

part1 :: Input -> String
part1 = go 0
  where
    go :: Int -> Input -> String
    go sec points 
      | yRange points < 15 = draw points
      | otherwise = go (sec + 1) $ move points
        where
          move = map (\(x, y, dx, dy) -> (x+dx, y+dy, dx, dy))
          ys = map (\(_, y, _, _) -> y)
          minY p = minimum $ ys p
          maxY p = maximum $ ys p
          yRange p = abs $ maxY p - minY p 
    

-- Part 2
          
part2 :: Input -> Int
part2 = go 0
  where
    go :: Int -> Input -> Int
    go sec points 
      | yRange points < 15 = sec
      | otherwise = go (sec + 1) $ move points
        where
          move = map (\(x, y, dx, dy) -> (x+dx, y+dy, dx, dy))
          ys = map (\(_, y, _, _) -> y)
          minY p = minimum $ ys p
          maxY p = maximum $ ys p
          yRange p = abs $ maxY p - minY p 
