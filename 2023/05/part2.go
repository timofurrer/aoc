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
		
		if strings.HasPrefix(line, "seeds: ") {
			// first line with seeds
			s, _ := strings.CutPrefix(line, "seeds: ")
			l := aoc.ParseInt64List(s)
			for i := 0; i < len(l); i+=2 {
				for j := int64(0); j < l[i+1]; j++ {
					x := l[i] + j
					seedPaths[x] = []int64{x}
				}
			}
			// fmt.Printf("Found seeds len=%d: %+v\n", len(seedPaths), seedPaths)
			fmt.Printf("found seeds")
		}

		if strings.HasSuffix(line, "map:") {
			fmt.Printf("New Map: %s\n", line)
			for k, p := range seedPaths {
				// if k == 13 {
				// 	fmt.Printf("  P: %+v (len=%d), mapIdx=%d\n", p, len(p), mapIdx)
				// }
				if len(p) < mapIdx {
					seedPaths[k] = append(p, p[len(p)-1])
					// if k == 13 {
					// 	// fmt.Printf("  Didn't find seed, adding last: %d -> %+v\n", p[len(p)-1], seedPaths[k])
					// }
				}
			}
			mapIdx++
			continue
		}

		for k, p := range seedPaths {
			// if k == 13 {
			// 	fmt.Printf("Checking seed paths: %d (p=%+v) and map %d\n", k, p, mapIdx)
			// }
			if len(p) >= mapIdx {
				// if k == 13 {
				// 	fmt.Printf("  P: %+v, breaking\n", p)
				// }
				continue
			}
			parts := aoc.ParseInt64List(line)
			dstStart := parts[0]
			// dstEnd := dstStart + parts[2]
			srcStart := parts[1]
			srcEnd := srcStart + parts[2]
			// if k == 13 {
			// 	fmt.Printf("  Checking: %d >= %d && %d < %d\n", p[len(p)-1], srcStart, p[len(p) -1], srcEnd)
			// }
			if p[len(p)-1] >= srcStart && p[len(p)-1] < srcEnd {
				seedPaths[k] = append(p, dstStart + p[len(p)-1] - srcStart)
				// if k == 13 {
				// 	fmt.Printf("  Add new seed: %d -> %+v\n", dstStart + p[len(p)-1] - srcStart, seedPaths[k])
				// }
			}
		}
	}
	// fmt.Printf("Seed paths: %+v\n", seedPaths)
	return aoc.Min(aoc.Map(func(x []int64) int64 { return x[len(x)-1]}, maps.Values(seedPaths))...)
}
