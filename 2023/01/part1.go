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

	answer := partOne(input)

	fmt.Printf("Part 1: %d\n", answer)
}

func partOne(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	calibrationValues := []int64{}
	for scanner.Scan() {
		line := scanner.Text()
		nums := aoc.FilterMap(func(c rune) (int64, bool) {
			if c >= '0' && c <= '9' {
				n := aoc.Int64(string(c))
				return n, true
			}
			return 0, false
		}, []rune(line))
		first := nums[0]
		last := nums[len(nums)-1]
		calibrationValues = append(calibrationValues, first*10+last)
	}

	answer := aoc.Sum(calibrationValues)
	return answer
}
