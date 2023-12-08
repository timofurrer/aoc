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

type hand struct {
	cards []rune
	cardCount aoc.Counter[rune]
	bid int64
}

var lrMap = map[rune]int{
	'L': 0,
	'R': 1,
}

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	var lrs []int
	network := make(map[string][]string)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		line = strings.ReplaceAll(strings.ReplaceAll(strings.ReplaceAll(line, "(", ""), ",", ""), ")", "")
		fields := strings.Fields(line)
		if len(fields) == 1 {
			lrs = aoc.Map(func(x rune) int { return lrMap[x] }, []rune(fields[0]))
			continue
		} 

		network[fields[0]] = []string{fields[2], fields[3]}
	}

	current := "AAA"
	lrIdx := 0
	for current != "ZZZ" {
		current = network[current][lrs[lrIdx % len(lrs)]]
		lrIdx += 1
	}

	return lrIdx
}
