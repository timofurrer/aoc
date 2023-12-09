package main

import (
	"bufio"
	"fmt"
	"io"
	"os"

	aoc "github.com/timofurrer/aoc/lib/go"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	var sum int64
	for scanner.Scan() {
		line := scanner.Text()
		seq := aoc.ParseInt64List(line)

		historySeqs := [][]int64{seq}
		for !aoc.AllEq(aoc.Last(historySeqs)...) {
			l := aoc.Last(historySeqs)
			h := []int64{}
			for i := 0; i < len(l) - 1; i++ {
				h = append(h, l[i+1] - l[i])
			}
			historySeqs = append(historySeqs, h)
		}

		sum += aoc.ReduceReverse(func(acc int64, x []int64) int64 { return acc + aoc.Last(x)}, historySeqs, 0)
	}
	return sum
}
