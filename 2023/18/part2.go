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
	'0': 1 + 0i,
	'2': -1 + 0i,
	'3': 0 + -1i,
	'1': 0 + 1i,
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)
 
	plan := []aoc.Point2d{0 + 0i}
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Fields(line)
		h := strings.Trim(fields[2], "(#)")

		d := rune(h[5])
		n := aoc.IntFromHex(h[:5])

		for i := 0; i < n; i++ {
			p := aoc.Last(plan) + dir[d]
			plan = append(plan, p)
		}
	}

	trench := aoc.EnclosedAreaWithinPath(plan) + int64(len(plan[1:]))
	return trench
}
