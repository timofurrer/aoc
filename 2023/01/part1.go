package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

func main() {
	input := os.Stdin

	answer := partOne(input)

	fmt.Printf("Part 1: %d\n", answer)
}

func partOne(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)
	scanner.Split(bufio.ScanLines)

	calibrationValues := []int64{}
	for scanner.Scan() {
		line := scanner.Text()
		nums := []int64{}
		for _, c := range line {
			if c >= '0' && c <= '9' {
				i, err := strconv.ParseInt(string(c), 10, 64)
				if err != nil {
					panic("mep")
				}

				nums = append(nums, i)
			}
		}
		first := nums[0]
		last := nums[len(nums)-1]
		calibrationValues = append(calibrationValues, first*10+last)
	}

	answer := int64(0)
	for _, i := range calibrationValues {
		answer += i
	}

	return answer
}
