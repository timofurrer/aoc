package main

import (
	"fmt"
	"io"
	"os"

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

	frontier := []aoc.Pair[aoc.Point2d, aoc.Set[aoc.Point2d]]{
		aoc.NewPair(start, aoc.Set[aoc.Point2d]{}),
	}

	paths := []int{}
	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]
		position := current.A
		path := current.B

		if position == end {
			paths = append(paths, len(path))
			continue
		}

		path.Add(position)
		for _, nd := range aoc.Neighbors2d4 {
			np := position + nd
			if n, ok := grid[np]; ok && n != '#' {
				if !path.Contains(np) {
					frontier = append(frontier, aoc.NewPair(np, path.Copy()))
				}
			}
		}
	}

	return aoc.Max(paths...)
}
