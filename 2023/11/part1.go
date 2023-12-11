package main

import (
	"bufio"
	"fmt"
	"io"
	"os"

	aoc "github.com/timofurrer/aoc/lib/go"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	grid := [][]int{}
	galaxyIdx := 0
	for scanner.Scan() {
		line := scanner.Text()

		r := []int{}
		for _, x := range line {
			if x != '#' {
				r = append(r, 0)
			} else {
				galaxyIdx++
				r = append(r, galaxyIdx)
			}
		}
		grid = append(grid, r)
	}

	allEmpty := func(x int) bool { return x == 0 }
	emptyRows := []int{}
	for r, row := range grid {
		if aoc.AllFunc(allEmpty, row...) {
			emptyRows = append(emptyRows, r)
		}
	}
	emptyCols := []int{}
	for c, col := range aoc.Transpose(grid) {
		if aoc.AllFunc(allEmpty, col...) {
			emptyCols = append(emptyCols, c)
		}
	}

	galaxies := []aoc.Point2d{}
	emptyMultiplier := 2
	for y, row := range grid {
		yOffset := len(aoc.Filter(func(yP int) bool { return yP < y}, emptyRows)) * (emptyMultiplier-1)
		for x, c := range row {
			xOffset := len(aoc.Filter(func(xP int) bool { return xP < x}, emptyCols)) * (emptyMultiplier-1)
			if c > 0 {
				galaxies = append(galaxies, aoc.NewPoint2d(x + xOffset, y + yOffset))
			}
		}
	}

	return aoc.Sum(aoc.Map(func(p aoc.Pair[aoc.Point2d, aoc.Point2d]) int64 { return int64(aoc.ManhattenDistance(p.A, p.B))}, aoc.DistinctValueCombinations(galaxies)))
}