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

	cards := []aoc.Pair[int, int]{}
	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, ":")
		cardSets := strings.Split(p[1], "|")
		winning := aoc.NewSet(aoc.ParseInt64List(cardSets[0])...)
		mine := aoc.NewSet(aoc.ParseInt64List(cardSets[1])...)
		intersection := mine.Intersection(winning)
		cards = append(cards, aoc.NewPair(1, len(intersection)))
	}

	for c, card := range cards {
		if card.B > 0 {
			for i := 0; i < card.A; i++ {
				for j := 0; j < card.B; j++ {
					if k := c + j + 1; k < len(cards) {
						cards[k].A += 1
					}
				}
			}
		}
	}

	return aoc.Reduce(func(acc int, x aoc.Pair[int, int]) int { return acc + x.A}, cards, 0)
}
