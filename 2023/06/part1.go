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

	times := []int64{}
	distances := []int64{}
	for scanner.Scan() {
		line := scanner.Text()

		if x, found := strings.CutPrefix(line, "Time: "); found {
			times = aoc.ParseInt64List(x)
		}
		if x, found := strings.CutPrefix(line, "Distance: "); found {
			distances = aoc.ParseInt64List(x)
		}
	}

	races := aoc.Zip(times, distances)
	racesWinning := []int{}
	for _, race := range races {
		time := race[0]
		recordDistance := race[1]

		winning := []int64{}

		for i := int64(0); i <= time; i++ {
			distance := i * (time - i)
			if distance > recordDistance {
				winning = append(winning, i)
			}
		}
		racesWinning = append(racesWinning, len(winning))
	}
	return aoc.Mul(racesWinning)
}
