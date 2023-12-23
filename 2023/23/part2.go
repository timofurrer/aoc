package main

import (
	"fmt"
	"io"
	"os"
	"slices"

	aoc "github.com/timofurrer/aoc/lib/go"
	"golang.org/x/exp/maps"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

func solve(input io.Reader) int {
	grid := aoc.ParseGrid2d(input, func(x rune) rune { return x })
	boundX := aoc.Max(aoc.Map(func(p aoc.Point2d) int64 { return p.X() }, maps.Keys(grid))...)
	boundY := aoc.Max(aoc.Map(func(p aoc.Point2d) int64 { return p.Y() }, maps.Keys(grid))...)
	start := aoc.NewPoint2d(1, 0)
	end := aoc.NewPoint2d(boundX-1, boundY)

	frontier := []aoc.Pair[aoc.Point2d, []aoc.Point2d]{
		aoc.NewPair(start, []aoc.Point2d{}),
	}

	paths := [][]aoc.Point2d{}
	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]
		position := current.A
		path := current.B

		if slices.Contains(path, position) {
			continue
		}

		newPath := append(aoc.Copy(path), position)

		if position == end {
			paths = append(paths, newPath)
			continue
		}

		switch grid[position] {
		default:
			for _, nd := range aoc.Neighbors2d4 {
				np := position + nd
				if n, ok := grid[np]; ok && n != '#' {
					frontier = append(frontier, aoc.NewPair(np, newPath))
				}
			}
		}
	}

	lengths := aoc.Map(func(p []aoc.Point2d) int { return len(p) - 1}, paths)
	return aoc.Max(lengths...)
}
