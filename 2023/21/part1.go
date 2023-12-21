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
	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]

		position := aoc.Last(current)

		if len(current) == 65 {
			canReach.Add(position)
			continue
		}

		ns := aoc.Filter(func(np aoc.Point2d) bool {
			if n, ok := grid[np]; ok {
				return n != '#'
			}
			return false
		}, aoc.AllNeighborCoords(position, aoc.Neighbors2d4))

		frontier = append(frontier, aoc.Map(func(n aoc.Point2d) []aoc.Point2d {
			return aoc.Copy(append(current, n))
		}, ns)...)
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
