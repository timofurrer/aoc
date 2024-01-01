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

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	graph := map[string]aoc.Set[string]{}
	for scanner.Scan() {
		line := scanner.Text()

		parts := strings.Split(line, ": ")
		from := parts[0]
		tos := strings.Fields(parts[1])
		for _, to := range tos {
			if g, ok := graph[from]; ok {
				g.Add(to)
			} else {
				graph[from] = aoc.NewSet(to)
			}
			if g, ok := graph[to]; ok {
				g.Add(from)
			} else {
				graph[to] = aoc.NewSet(from)
			}
		}
	}

	S := aoc.NewSet(maps.Keys(graph)...)
	count := func(v string) int { return len(graph[v].Difference(S)) }
	for aoc.SumMap(count, S.ToSlice()) != 3 {
		S.Del(aoc.MaxWithKey(count, S.ToSlice()...))
	}

	return len(S) * len(aoc.NewSet(maps.Keys(graph)...).Difference(S))
}