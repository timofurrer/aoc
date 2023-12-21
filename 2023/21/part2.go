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
	boundX := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.X() }, maps.Keys(grid))...)
	boundY := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.Y() }, maps.Keys(grid))...)

	canMove := func(p aoc.Point2d) bool {
		c := accessInfiniteGrid(grid, boundX+1, boundY+1, p)
		return c != '#'
	}

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

		// fmt.Printf("Looking at %+v current with len %d\n", current, len(current))

		if len(current) == (65 + 262 + 1) {
			canReach.Add(position)
			continue
		}

		for _, nd := range aoc.Neighbors2d4 {
			np := position + nd
			if canMove(np) {
				frontier = append(frontier, append(aoc.Copy(current), np))
			}
		}
	}

	// 2722
	// 25621

	fmt.Printf("Can Reach: %+v\n", canReach)

	return len(canReach)
}

func accessInfiniteGrid(grid aoc.Grid2d[aoc.Point2d, rune], boundX int64, boundY int64, p aoc.Point2d) rune {
	ip := aoc.NewPoint2d((p.X() % boundX + boundX) % boundX, (p.Y() % boundY + boundY) % boundY)
	if c, ok := grid[ip]; ok {
		return c
	}
	fmt.Printf("ip: %+v for p %+v, boundX=%d, boudnY=%d\n", ip, p, boundX, boundY)
	panic("whuat")
}

func findStart(grid aoc.Grid2d[aoc.Point2d, rune]) aoc.Point2d {
	for p, c := range grid {
		if c == 'S' {
			return p
		}
	}
	panic("unable to find start")
}
