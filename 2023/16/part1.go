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

var backMirrorChange = map[aoc.Point2d]aoc.Point2d{ // mirror: \
	1 + 0i: 0 + 1i,
	-1 + 0i: 0 + -1i,
	0 + 1i: 1 + 0i,
	0 + -1i: -1 + 0i,
}

var frontMirrorChange = map[aoc.Point2d]aoc.Point2d{ // mirror: /
	1 + 0i: 0 + -1i,
	-1 + 0i: 0 + 1i,
	0 + 1i: -1 + 0i,
	0 + -1i: 1 + 0i,
}

func solve(input io.Reader) int {
	grid := aoc.ParseGrid2d(input, func(x rune) rune { return x })
	boundX := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.X() }, maps.Keys(grid))...)
	boundY := aoc.Max(aoc.Map(func (p aoc.Point2d) int64 { return p.Y() }, maps.Keys(grid))...)

	beams := []beam{{pos: 0 + 0i, dir: 1 + 0i}}
	energizedTiles := aoc.Set[aoc.Point2d]{}
	for sameTiles := 0; sameTiles <= 10; {
		energized := len(energizedTiles)
		newBeams := []beam{}
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
					
					newBeams = append(newBeams, beam{pos: b.pos + (-1 + 0i), dir: -1 + 0i})
				}
			case '|':
				if b.dir.X() == 0 {
					beams[i].pos = b.pos + b.dir
				} else {
					beams[i].dir = 0 + 1i
					beams[i].pos = b.pos + beams[i].dir
					
					newBeams = append(newBeams, beam{pos: b.pos + (0 + -1i), dir: 0 + -1i})
				}
			}
		}
		beams = append(beams, newBeams...)
		beams = aoc.Filter(func (b beam) bool {
			return b.pos.X() >= 0 && b.pos.X() <= boundX &&
				b.pos.Y() >= 0 && b.pos.Y() <= boundY
		}, beams)

		if energized == len(energizedTiles) { 
			sameTiles++
		} else {
			sameTiles = 0
		}
	}
	
	return len(energizedTiles)
}