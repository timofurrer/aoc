package main

import (
	"container/heap"
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

type block struct {
	pos aoc.Point2d
	heat int
	dir aoc.Point2d
}

var directions = aoc.NewSet(
	aoc.NewPoint2d(1, 0),
	aoc.NewPoint2d(0, 1),
	aoc.NewPoint2d(-1, 0),
	aoc.NewPoint2d(0, -1),
)

func solve(input io.Reader) int {
	grid := aoc.ParseGrid2d(input, func(x rune) int { return int(x) - 48 })
	boundX := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.X() }, maps.Keys(grid))...)
	boundY := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.Y() }, maps.Keys(grid))...)

	start := aoc.NewPoint2d(0, 0)
	goal := aoc.NewPoint2d(boundX, boundY)
	heatLoss := leastHeatLoss(grid, start, goal)
	
	return heatLoss
}

func leastHeatLoss(grid aoc.Grid2d[aoc.Point2d, int], start aoc.Point2d, end aoc.Point2d) int {
	frontier := aoc.PriorityQueue[block]{
		&aoc.Item[block]{
			Value: block{
				pos: start,
				heat: 0,
				dir: 0 + 0i,
			},
			Priority: 0,
		},
	}
	heap.Init(&frontier)
	seen := aoc.Set[aoc.Pair[aoc.Point2d, aoc.Point2d]]{}

	for frontier.Len() > 0 {
		current := heap.Pop(&frontier).(*aoc.Item[block])
		if current.Value.pos == end {
			return current.Value.heat
		}

		if p := aoc.NewPair(current.Value.pos, current.Value.dir); seen.Contains(p) {
			continue
		} else {
			seen.Add(p)
		}

		ds := directions.Difference(aoc.NewSet(current.Value.dir, aoc.NewPoint2d(-current.Value.dir.X(), -current.Value.dir.Y())))
		for _, d := range ds.ToSlice() {
			p := current.Value.pos
			h := current.Value.heat
			for i := 0; i < 10; i++ {
				p += d
				if hP, found := grid[p]; found {
					h += hP
					if i >= 3 {
						heap.Push(&frontier, &aoc.Item[block]{
							Value: block{pos: p, dir: d, heat: h},
							Priority: h,
						})
					}
				}
			}
		}
	}

	return -1
}