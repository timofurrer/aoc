package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func main() {
	input := os.Stdin

	answer := partTwo(input)

	fmt.Printf("Part 2: %d\n", answer)
}

var numbers = []string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

func partTwo(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)
	scanner.Split(bufio.ScanLines)

	calibrationValues := []int64{}
	for scanner.Scan() {
		line := scanner.Text()
		nums := []int64{}
		for i, c := range line {
			if c >= '0' && c <= '9' {
				n, err := strconv.ParseInt(string(c), 10, 64)
				if err != nil {
					panic("mep")
				}

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

	answer := int64(0)
	for _, i := range calibrationValues {
		answer += i
	}

	return answer
}
