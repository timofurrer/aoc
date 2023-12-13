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

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	rawPatterns := [][][]rune{}
	rawPattern := [][]rune{}
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			rawPatterns = append(rawPatterns, rawPattern)
			rawPattern = [][]rune{}
			continue
		}

		rawPattern = append(rawPattern, []rune(line))
	}
	rawPatterns = append(rawPatterns, rawPattern)

	notes := 0
	for _, rawPattern := range rawPatterns {
		notes += reflection(rawPattern, 0) * 100
		notes += reflection(aoc.Transpose(rawPattern), 0)
	}

	return notes
}

func reflection(pattern [][]rune, maxDiff int) int {
	for i, row := range pattern {
		if i != len(pattern) - 1 {
			l := len(aoc.Filter(func(x []rune) bool { return x[0] != x[1] }, aoc.Zip(row, pattern[i+1])))
			if l <= maxDiff {
				s := aoc.Sum(aoc.Map(diff, aoc.Zip(aoc.Reverse(pattern[:i]), pattern[i+2:])))
				if s == maxDiff - l {
					return i+1
				}
			}
		}
	}
	return 0
}

func diff(xy [][]rune) int {
	return len(aoc.Filter(func (f []rune) bool {
		return f[0] != f[1]
	}, aoc.Zip(xy[0], xy[1])))
}