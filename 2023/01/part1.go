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
		nums := []int64{}
		for _, c := range line {
			if c >= '0' && c <= '9' {
				n := aoc.Int64(string(c))
				nums = append(nums, n)
			}
		}
		first := nums[0]
		last := nums[len(nums)-1]
		calibrationValues = append(calibrationValues, first*10+last)
	}

	answer := aoc.Sum(calibrationValues)
	return answer
}
