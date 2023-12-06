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
func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	seedRanges := []aoc.Pair[int64, int64]{}
	mappings := make([][][]int64, 7)
	mapIdx := -1
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			continue
		}

		if s, found := strings.CutPrefix(line, "seeds: "); found {
			l := aoc.ParseInt64List(s)
			for i := 0; i < len(l); i += 2 {
				seedRanges = append(seedRanges, aoc.NewPair(l[i], l[i]+l[i+1]-1))
			}
			continue
		}

		if strings.HasSuffix(line, "map:") {
			mapIdx++
			continue
		}

		mappings[mapIdx] = append(mappings[mapIdx], aoc.ParseInt64List(line))
	}
	for _, mapping := range mappings {
		newSeedRanges := []aoc.Pair[int64, int64]{}
		for _, mappingRange := range mapping {
			start := mappingRange[1]
			end := mappingRange[1] + mappingRange[2] - 1
			cutRanges := []aoc.Pair[int64, int64]{}
			for _, r := range seedRanges {
				if start < r.B && end >= r.A {
					cutStart := aoc.Max(start, r.A)
					cutEnd := aoc.Min(end, r.B)
					newSeedRanges = append(newSeedRanges, aoc.NewPair(
						cutStart + mappingRange[0] - mappingRange[1], 
						cutEnd + mappingRange[0] - mappingRange[1],
					))
					if r.A < cutEnd {
						cutRanges = append(cutRanges, aoc.NewPair(r.A, cutStart-1))
					}
					if r.B > cutEnd {
						cutRanges = append(cutRanges, aoc.NewPair(cutEnd+1, r.B))
					}
				} else {
					cutRanges = append(cutRanges, r)
				}
			}
			seedRanges = cutRanges
		} 
		seedRanges = newSeedRanges
	}
	
	return aoc.Min(aoc.Map(func(x aoc.Pair[int64, int64]) int64 { return x.A }, seedRanges)...)
}
