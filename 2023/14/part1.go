package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
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

	load := 0
	maxHeight := len(grid)
	for x := 0; x < len(grid[0]); x++ {
		rolledRocks := 0
		for y := maxHeight - 1; y >= 0; y-- {
			switch grid[y][x] {
			case '#':
				load += rockLoad(maxHeight - 1 - y, rolledRocks)
				rolledRocks = 0
			case 'O':
				rolledRocks += 1
			}
		}
		load += rockLoad(maxHeight, rolledRocks)
	}

	return load
}

func rockLoad(stop int, rocks int) int {
	load := 0
	for i := 0; i < rocks; i++ {
		load += stop - i
	}
	return load
}