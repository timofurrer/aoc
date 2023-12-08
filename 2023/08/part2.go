package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
	"golang.org/x/exp/maps"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
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

	paths := aoc.Filter(func(x string) bool { return strings.HasSuffix(x, "A") }, maps.Keys(network))
	endAfter := make([][]int, 0, len(lrs))
	for range lrs {
		endAfter = append(endAfter, make([]int, len(paths)))
	}
	lrIdx := 0
	for aoc.All(aoc.Map(func(xs []int) bool { return aoc.AnyFunc(func(x int) bool { return x == 0 }, xs...) }, endAfter)...) {
		lr := lrs[lrIdx % len(lrs)]
		for i := 0; i < len(paths); i++ {
			if aoc.AllFunc(func (xs []int) bool { return xs[i] > 0 }, endAfter...) {
				continue
			}

			paths[i] = network[paths[i]][lr]
			if strings.HasSuffix(paths[i], "Z") {
				endAfter[lr][i] = lrIdx + 1
			}
		}
		lrIdx += 1
	}
	
	if aoc.AllFunc(func(x int) bool { return x > 0 }, endAfter[0]...) {
		return aoc.LCM(endAfter[0]...)
	} else {
		return aoc.LCM(endAfter[1]...)
	}
}