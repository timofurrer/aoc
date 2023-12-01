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

	answer := partTwo(input)

	fmt.Printf("Part 2: %d\n", answer)
}

var numbers = []string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

func partTwo(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	calibrationValues := []int64{}
	for scanner.Scan() {
		line := scanner.Text()
		nums := []int64{}
		for i, c := range line {
			if c >= '0' && c <= '9' {
				n := aoc.Int64(string(c))
				nums = append(nums, n)
				continue
			}

			for n, number := range numbers {
				if strings.HasPrefix(line[i:], number) {
					nums = append(nums, int64(n+1))
				}
			}
		}
		first := nums[0]
		last := nums[len(nums)-1]
		calibrationValues = append(calibrationValues, first*10+last)
	}

	answer := aoc.Sum(calibrationValues)

	return answer
}
