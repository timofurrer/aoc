package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/mitchellh/hashstructure/v2"
	aoc "github.com/timofurrer/aoc/lib/go"
)

type cacheKey struct {
	A string
	B bool
	C int64
	D []int64
}

var runCache = map[uint64]int64{}

func runWithCache(recording string, inGroup bool, currentGroupSize int64, groups []int64) int64 {
	key := cacheKey{A: recording, B: inGroup, C: currentGroupSize, D: groups}
	h, err := hashstructure.Hash(key, hashstructure.FormatV2, nil)
	if err != nil {
		panic("unable to create hash")
	}
	if cachedResult, found := runCache[h]; found {
		return cachedResult
	}

	result := run(recording, inGroup, currentGroupSize, groups)

	runCache[h] = result
	return result
}

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	var possibilities int64
	for scanner.Scan() {
		line := scanner.Text()

		parts := strings.Split(line, " ")
		recording := parts[0]
		groupsDamaged := aoc.Map(func(x string) int64 { return aoc.Int64(x) }, strings.Split(parts[1], ","))

		unfoldedRecordings := string(aoc.RepeatWithSeparator([]rune(recording), 5, '?'))
		unfoldedGroups := aoc.Repeat(groupsDamaged, 5)

		possibilities += runWithCache(unfoldedRecordings, false, -1, unfoldedGroups)
	}

	return possibilities
}

func run(recording string, inGroup bool, currentGroupSize int64, groups []int64) int64 {
	if len(recording) == 0 {
		if inGroup {
			if len(groups) == 1 && currentGroupSize == groups[0] {
				return 1
			}
		} else {
			if len(groups) == 0 {
				return 1
			}
		}
		return 0
	}

	remainingPossibilies := aoc.Reduce(func(acc int64, c rune) int64 { 
		if c == '#' || c == '?' {
			return acc + 1
		} 
		return acc
	}, []rune(recording), 0)

	groupTotal := aoc.Sum(groups)
	if inGroup {
		if remainingPossibilies + currentGroupSize < groupTotal {
			return 0
		}
		if len(groups) == 0 {
			return 0
		}
	} else {
		if remainingPossibilies < groupTotal {
			return 0
		}
	}

	var possibilities int64
	c := recording[0]
	if inGroup {
		if c == '.' && currentGroupSize != groups[0] {
			return 0
		}
		if c == '.' {
			possibilities += runWithCache(recording[1:], false, -1, groups[1:])
		}
		if c == '?' && currentGroupSize == groups[0] {
			possibilities += runWithCache(recording[1:], false, -1, groups[1:])
		}
		if c == '#' || c == '?' {
			possibilities += runWithCache(recording[1:], true, currentGroupSize + 1, groups)
		}
	} else {
		if c == '#' || c == '?' {
			possibilities += runWithCache(recording[1:], true, 1, groups)
		}
		if c == '.' || c == '?' {
			possibilities += runWithCache(recording[1:], false, -1, groups)
		}

	}
	return possibilities
}
