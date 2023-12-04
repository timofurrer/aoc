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

	cards := [][]int{}
	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, ":")
		cardSets := strings.Split(p[1], "|")
		winning := aoc.NewSet(aoc.ParseInt64List(cardSets[0])...)
		mine := aoc.NewSet(aoc.ParseInt64List(cardSets[1])...)
		intersection := mine.Intersection(winning)
		cards = append(cards, []int{1, len(intersection)})
	}

	for c, card := range cards {
		if card[1] > 0 {
			for i := 0; i < card[0]; i++ {
				for j := 0; j < card[1]; j++ {
					if k := c + j + 1; k < len(cards) {
						cards[k][0] += 1
					}
				}
			}
		}
	}

	return aoc.Reduce(func(acc int, x []int) int { return acc + x[0]}, cards, 0)
}
