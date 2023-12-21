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

	x0 := getPlotsForSteps(start, canMove, int(start.X()))
	x1 := getPlotsForSteps(start, canMove, int(start.X() + boundX + 1))
	x2 := getPlotsForSteps(start, canMove, int(start.X() + boundX + 1 + boundX + 1))

	maxRechablePlotsAtX := (26501365 - int(start.X())) / int(boundX + 1) // 202300

	// These numbers are x = 0, x = 1, x = 2 of a quadratic function to calculate the
	// max rechable plots at 202300 = (26501365 - 65) / 131

	// Manually go to https://www.wolframalpha.com/input?i=quadratic+fit+calculator&assumption=%7B%22F%22,+%22QuadraticFitCalculator%22,+%22data3x%22%7D+-%3E%22%7B0,+1,+2%7D%22&assumption=%7B%22F%22,+%22QuadraticFitCalculator%22,+%22data3y%22%7D+-%3E%22%7B3884,+34564,+95816%7D%22
	// and get quadratic fit and solve for f(maxReachablePlotsAtX)

	fmt.Printf("f(x=0) = %d\n", x0)
	fmt.Printf("f(x=1) = %d\n", x1)
	fmt.Printf("f(x=2) = %d\n", x2)
	fmt.Printf("f(x=%d) = ???\n", maxRechablePlotsAtX)

	// 65             = 3884
	// 65 + 131       = 34564
	// 65 + 131 + 131 = 95816

	return 0
}

func getPlotsForSteps(start aoc.Point2d, canMove func(aoc.Point2d) bool, steps int) int {
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

		if len(current) == 1 + steps {
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
