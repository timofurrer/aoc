package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
)


func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

var dir = map[rune]aoc.Point2d{
	'R': 1 + 0i,
	'L': -1 + 0i,
	'U': 0 + -1i,
	'D': 0 + 1i,
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)
 
	plan := []aoc.Point2d{0 + 0i}
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Fields(line)

		d := rune(fields[0][0])
		n := aoc.Int(fields[1])

		for i := 0; i < n; i++ {
			p := aoc.Last(plan) + dir[d]
			plan = append(plan, p)
		}
	}

	trench := aoc.EnclosedAreaWithinPath(plan) + int64(len(plan[1:]))
	return trench
}
