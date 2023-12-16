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

type beam struct {
	pos aoc.Point2d
	dir aoc.Point2d
	pre aoc.Point2d
}

var backMirrorChange = map[aoc.Point2d]aoc.Point2d{
	1 + 0i: 0 + 1i,
	-1 + 0i: 0 + -1i,
	0 + 1i: 1 + 0i,
	0 + -1i: -1 + 0i,
}

var frontMirrorChange = map[aoc.Point2d]aoc.Point2d{
	1 + 0i: 0 + -1i,
	-1 + 0i: 0 + 1i,
	0 + 1i: -1 + 0i,
	0 + -1i: 1 + 0i,
}

func solve(input io.Reader) int {
	grid := aoc.ParseGrid2d(input, func(x rune) rune { return x })
	boundX := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.X() }, maps.Keys(grid))...)
	boundY := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.Y() }, maps.Keys(grid))...)

	// top
	allEnergized := []int{}
	for x := int64(0); x < boundX; x++ {
		fmt.Printf("Doing top %d, %d\n", x, boundX)
		allEnergized = append(allEnergized, energize(grid, boundX, boundY, beam{
			pos: aoc.NewPoint2d(x, 0),
			dir: 0 + 1i,
		}))
	}
	// bottom
	for x := int64(0); x < boundX; x++ {
		fmt.Printf("Doing bottom %d, %d\n", x, boundX)
		allEnergized = append(allEnergized, energize(grid, boundX, boundY, beam{
			pos: aoc.NewPoint2d(x, boundY),
			dir: 0 + -1i,
		}))
	}
	// left
	for y := int64(0); y < boundY; y++ {
		fmt.Printf("Doing left %d, %d\n", y, boundY)
		allEnergized = append(allEnergized, energize(grid, boundX, boundY, beam{
			pos: aoc.NewPoint2d(0, y),
			dir: 1 + 0i,
		}))
	}
	// right
	for y := int64(0); y < boundY; y++ {
		fmt.Printf("Doing right %d, %d\n", y, boundY)
		allEnergized = append(allEnergized, energize(grid, boundX, boundY, beam{
			pos: aoc.NewPoint2d(boundX, y),
			dir: -1 + 0i,
		}))
	}

	return aoc.Max(allEnergized...)
}

func energize(grid aoc.Grid2d[aoc.Point2d, rune], gridBoundX int64, gridBoundY int64, start beam) int{
	beams := []beam{start}
	energizedTiles := aoc.Set[aoc.Point2d]{}
	for sameTiles := 0; sameTiles <= 5; {
		// I guess we could optimize this a LOT by memoizing the individual beams and 
		// calculating how much tiles it energizes.
		beamsBefore := aoc.NewSet(aoc.Copy(beams)...)
		energized := len(energizedTiles)
		splitBeams := []beam{}
		for i, b := range beams {
			energizedTiles.Add(b.pos)
			posTile := grid[b.pos]
			switch posTile {
			case '.':
				beams[i].pos = b.pos + b.dir
			case '\\':
				beams[i].dir = backMirrorChange[b.dir]
				beams[i].pos = b.pos + beams[i].dir
			case '/':
				beams[i].dir = frontMirrorChange[b.dir]
				beams[i].pos = b.pos + beams[i].dir
			case '-':
				if b.dir.Y() == 0 {
					beams[i].pos = b.pos + b.dir
				} else {
					beams[i].dir = 1 + 0i
					beams[i].pos = b.pos + beams[i].dir
					
					splitBeams = append(splitBeams, beam{pos: b.pos + (-1 + 0i), dir: -1 + 0i})
				}
			case '|':
				if b.dir.X() == 0 {
					beams[i].pos = b.pos + b.dir
				} else {
					beams[i].dir = 0 + 1i
					beams[i].pos = b.pos + beams[i].dir
					
					splitBeams = append(splitBeams, beam{pos: b.pos + (0 + -1i), dir: 0 + -1i})
				}
			}
		}
		newBeams := append(beams, splitBeams...)
		newBeams = aoc.Filter(func (b beam) bool {
			return b.pos.X() >= 0 && b.pos.X() <= gridBoundX &&
				b.pos.Y() >= 0 && b.pos.Y() <= gridBoundY
		}, newBeams)
		beams = aoc.NewSet(newBeams...).Difference(beamsBefore).ToSlice()

		if energized == len(energizedTiles) { 
			sameTiles++
		} else {
			sameTiles = 0
		}
	}
	
	return len(energizedTiles)
}