module BfsSpec (spec) where

-- Imports

--- HSpec & Test Helpers

import Test.Hspec

import Data.Grid

--- Standard Library

import Control.Monad (forM_)

import qualified Data.Map.Strict as SMap

--- Under Test

import Lib.Paths
import Lib.Grid

-- Specs

spec :: Spec
spec = do
  describe "BFS" $ do
    let testCases = [testGrid0, testGrid1, testGrid2, testGrid3]

    forM_ testCases $ \testGrid ->
      it ("finds all shorest paths to all targets: " ++ name testGrid) $ do
        let g = grid testGrid
            startPos = start testGrid
            expectedPaths = paths testGrid
            visited = bfs startPos (testGridAdjacents testGrid)
            targets = map fst . filter (\(_, v) -> v == 'U') $ allPointsWithValue g
            actualPaths = SMap.fromList $ filter (\(_, ps) -> not $ null ps) $ map (\t -> (t, reconstructPaths visited t)) targets

        actualPaths `shouldBe` expectedPaths
