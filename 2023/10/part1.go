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

var neighbors = map[rune][]aoc.Point2d{
	'|': {aoc.NewPoint2d(0, -1), aoc.NewPoint2d(0, 1)},
	'-': {aoc.NewPoint2d(1, 0), aoc.NewPoint2d(-1, 0)},
	'L': {aoc.NewPoint2d(0, -1), aoc.NewPoint2d(1, 0)},
	'J': {aoc.NewPoint2d(0, -1), aoc.NewPoint2d(-1, 0)},
	'7': {aoc.NewPoint2d(0, 1), aoc.NewPoint2d(-1, 0)},
	'F': {aoc.NewPoint2d(0, 1), aoc.NewPoint2d(1, 0)},
}

func solve(input io.Reader) int {
	pipes := aoc.ParseGrid2d(input, func(x rune) rune { return x })
	start := pipes.FindOrPanic('S')

	// choose direction manually
	// S := 'F' // examples
	S := 'J' // input
	neighbors['S'] = neighbors[S]

	path := pipes.Walk(start, start, func(cur, prev aoc.Point2d) aoc.Point2d {
		ns := aoc.AllNeighborCoords(cur, neighbors[pipes[cur]])
		if cur == start { // first step
			return ns[0]
		}

		return aoc.Filter(func(x aoc.Point2d) bool { return x != prev }, ns)[0]
	})
	return len(path) / 2
}
