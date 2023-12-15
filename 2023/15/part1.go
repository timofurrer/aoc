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

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)
 
	s := 0
	for scanner.Scan() {
		line := scanner.Text()
		ds := strings.Split(line, ",")
		s += aoc.SumMap(hash, ds)
	}

	return s
}

func hash(data string) int {
	return aoc.Reduce(func(acc int, x rune) int { return ((acc + int(x)) * 17) % 256 }, []rune(data), 0)
}