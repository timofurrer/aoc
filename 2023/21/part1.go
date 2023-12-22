package main

import (
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

func solve(input io.Reader) int {
	grid := aoc.ParseGrid2d(input, func(x rune) rune { return x })
	start := findStart(grid)

	frontier := [][]aoc.Point2d{{start}}

	canReach := aoc.Set[aoc.Point2d]{}
	seen := aoc.Set[aoc.Pair[aoc.Point2d, int]]{}
	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]
		position := aoc.Last(current)

		s := aoc.NewPair(position, len(current))
		if seen.Contains(s) {
			continue
		}
		seen.Add(s)

		if len(current) == 65 {
			canReach.Add(position)
			continue
		}

		for _, nd := range aoc.Neighbors2d4 {
			np := position + nd
			if n, ok := grid[np]; ok && n != '#' {
				frontier = append(frontier, append(aoc.Copy(current), np))
			}
		}
	}

	fmt.Printf("Can Reach: %+v\n", canReach)
	return len(canReach)
}

func findStart(grid aoc.Grid2d[aoc.Point2d, rune]) aoc.Point2d {
	for p, c := range grid {
		if c == 'S' {
			return p
		}
	}
	panic("unable to find start")
}
