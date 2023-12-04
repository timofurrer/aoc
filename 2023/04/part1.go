package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}
func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	values := []float64{}
	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, ":")
		cardSets := strings.Split(p[1], "|")
		winning := aoc.NewSet(aoc.ParseInt64List(cardSets[0])...)
		mine := aoc.NewSet(aoc.ParseInt64List(cardSets[1])...)
		intersection := mine.Intersection(winning)
		if len(intersection) > 0 {
			values = append(values, math.Pow(2, float64(len(intersection)-1)))
		}
	}
	return int64(aoc.Sum(values))
}
