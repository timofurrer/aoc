package main

import (
	"bufio"
	"cmp"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

type hand struct {
	cards []rune
	cardCount aoc.Counter[rune]
	bid int64
}

var cardValues = []rune{'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'}
var getCardValue = func(x rune) int { return slices.Index(cardValues, x)}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	hands := []hand{}
	for scanner.Scan() {
		line := scanner.Text()

		p := strings.Fields(line)
		cards := []rune(p[0])
		hands = append(hands, hand{
			cards, NewJokerCounter(cards), aoc.Int64(p[1]),
		})
	}

	slices.SortFunc(hands, func(x, y hand) int {
		hx, hy := getHandRank(x), getHandRank(y)
		if c := cmp.Compare(hx, hy); c != 0 {
			return c
		} else {
			for _, z := range aoc.Zip(x.cards, y.cards) {
				if cz := cmp.Compare(getCardValue(z[0]), getCardValue(z[1])); cz != 0 {
					return -cz
				}
			}
			return 0
		}
	})

	totalWinnings := int64(0)
	for i, h := range hands {
		totalWinnings += h.bid * int64(i + 1)
	}

	return totalWinnings
}

func NewJokerCounter(hand []rune) aoc.Counter[rune] {
	c := aoc.Counter[rune]{}
	h := aoc.Pair[rune, int]{A: 'X', B: -1}
	for _, r := range hand {
		c[r] += 1
		if r != 'J' && c[r] > h.B {
			h.A = r
			h.B = c[r]
		}
	}

	if j, ok := c['J']; ok {
		delete(c, 'J')
		c[h.A] += j
	}
	return c
}

func getHandRank(h hand) int {
	switch c := h.cardCount.Values(); {
	case isFiveOfAKind(c):
		return 7
	case isFourOfAKind(c):
		return 6
	case isFullHouse(c):
		return 5
	case isThreeOfAKind(c):
		return 4
	case isTwoPair(c):
		return 3
	case isOnePair(c):
		return 2
	case isHighCard(c):
		return 1
	}
	panic(fmt.Errorf("invalid hand: %+v", h))
}

func isFiveOfAKind(c []int) bool {
	return len(c) == 1
}

func isFourOfAKind(c []int) bool {
	return aoc.AnyFunc(func(v int) bool { return v == 4 }, c...)
}

func isThreeOfAKind(c []int) bool {
	return len(c) == 3 && c[0] == 1 && c[1] == 1 && c[2] == 3
}

func isFullHouse(c []int) bool {
	return len(c) == 2 && c[0] == 2 && c[1] == 3
}

func isTwoPair(c []int) bool {
	return len(c) == 3 && c[0] == 1 && c[1] == 2 && c[2] == 2
}

func isOnePair(c []int) bool {
	return len(c) == 4 && c[0] == 1 && c[1] == 1 && c[2] == 1 && c[3] == 2
}

func isHighCard(c []int) bool {
	return len(c) == 5
}