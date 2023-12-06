package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
	"golang.org/x/exp/maps"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}
func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	seedPaths := make(map[int64][]int64)
	mapIdx := 1
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			continue
		}
		
		if s, found := strings.CutPrefix(line, "seeds: "); found {
			for _, x := range aoc.ParseInt64List(s) {
				seedPaths[x] = []int64{x}
			}
		}

		if strings.HasSuffix(line, "map:") {
			for k, p := range seedPaths {
				if len(p) < mapIdx {
					seedPaths[k] = append(p, p[len(p)-1])
				}
			}
			mapIdx++
			continue
		}

		for k, p := range seedPaths {
			if len(p) >= mapIdx {
				continue
			}
			parts := aoc.ParseInt64List(line)
			dstStart := parts[0]
			srcStart := parts[1]
			srcEnd := srcStart + parts[2]
			if cur := p[len(p)-1]; cur >= srcStart && cur < srcEnd {
				seedPaths[k] = append(p, dstStart + cur - srcStart)
			}
		}
	}
	return aoc.Min(aoc.Map(func(x []int64) int64 { return x[len(x)-1]}, maps.Values(seedPaths))...)
}
