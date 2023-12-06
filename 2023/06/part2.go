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
func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	var time, recordDistance int64
	for scanner.Scan() {
		line := scanner.Text()

		if x, found := strings.CutPrefix(line, "Time: "); found {
			time = aoc.Int64(strings.ReplaceAll(x, " ", ""))
		}
		if x, found := strings.CutPrefix(line, "Distance: "); found {
			recordDistance = aoc.Int64(strings.ReplaceAll(x, " ", ""))
		}
	}
	
	winning := []int64{}
	for i := int64(0); i <= time; i++ {
		distance := i * (time - i)
		if distance > recordDistance {
			winning = append(winning, i)
		}
	}
	
	return len(winning)
}
