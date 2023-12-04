package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"unicode"

	aoc "github.com/timofurrer/aoc/lib/go"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

type number struct {
	n      int64
	coords aoc.Set[aoc.Point2d]
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	symbolGrid := aoc.NewSet[aoc.Point2d]()
	numbers := []number{}
	for row := 0; scanner.Scan(); row++ {
		line := scanner.Text()
		curNum := []rune{}
		curCoords := aoc.NewSet[aoc.Point2d]()
		maybeRecordNumber := func() {
			if len(curNum) > 0 {
				numbers = append(numbers, number{n: aoc.Int64(string(curNum)), coords: curCoords})
				curNum = make([]rune, 0)
				curCoords = aoc.NewSet[aoc.Point2d]()
			}
		}
		for col, c := range line {
			if unicode.IsDigit(c) {
				curNum = append(curNum, c)
				curCoords.Add(aoc.NewPoint2d(row, col))
			} else {
				if c != '.' {
					symbolGrid.Add(aoc.NewPoint2d(row, col))
				}

				if len(curNum) > 0 {
					maybeRecordNumber()
				}
			}
		}
		maybeRecordNumber()
	}

	parts := []int64{}
	for _, n := range numbers {
		for c := range n.coords {
			if aoc.AnyFunc(func(d aoc.Point2d) bool {
				return symbolGrid.Contains(c + d)
			}, aoc.Neighbors2d8...) {
				parts = append(parts, n.n)
				break
			}
		}
	}

	return aoc.Sum(parts)
}
