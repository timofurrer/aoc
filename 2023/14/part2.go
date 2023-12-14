package main

import (
	"bufio"
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
	scanner := bufio.NewScanner(input)
 
	grid := [][]rune{}
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []rune(line))
	}

	cycles := make(map[aoc.Hashed]aoc.Pair[int, int])
	maxCycle := 1000000000
	for cycle := 1; cycle <= maxCycle; cycle++ {
		for i := 0; i < 4; i++ {
			grid = aoc.RotateCounterClockwise(tilt(grid))
		}

		h := aoc.Hash(grid)
		if c, found := cycles[h]; found {
			cycled := cycle - c.A
			for _, ce := range cycles {
				if ce.A >= c.A && ce.A % cycled == maxCycle % cycled {
					return ce.B
				}
			}
		}

		cycles[h] = aoc.NewPair(cycle, load(grid))
	}
	
	return -1
}

func tilt(grid [][]rune) [][]rune {
	tGrid := aoc.Copy(grid)
	for x := 0; x < len(grid[0]); x++ {
		moveY := 0
		for y := 0; y < len(grid); y++ {
			switch grid[y][x] {
			case '#':
				moveY = y + 1
			case 'O':
				tGrid[y][x] = '.'
				tGrid[moveY][x] = 'O'
				moveY++
			}
		}
	}
	return tGrid
}

func load(grid [][]rune) int {
	maxHeight := len(grid)
	load := 0
	for i, r := range grid {
		load += (maxHeight - i) * aoc.Count(r, 'O')
	}
	return load
}