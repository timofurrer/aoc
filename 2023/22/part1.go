package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
	"golang.org/x/exp/slices"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

type brick []cube

type cube struct {
	X, Y, Z int
}

func parseTriple(s string) aoc.Triple[int, int, int] {
	ps := strings.Split(s, ",")
	return aoc.NewTriple(aoc.Int(ps[0]), aoc.Int(ps[1]), aoc.Int(ps[2]))
}

func parseCube(s string) cube {
	ps := strings.Split(s, ",")
	return cube{aoc.Int(ps[0]), aoc.Int(ps[1]), aoc.Int(ps[2])}
}

func cmp(a, b int) int {
	if a < b {
		return -1
	} else if a > b {
		return 1
	}
	return 0
}

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	bricks := []brick{}
	for scanner.Scan() {
		line := scanner.Text()

		parts := strings.Split(line, "~")

		from := parseCube(parts[0])
		to := parseCube(parts[1])
		brick := []cube{from, to}

		if from.X != to.X {
			for j := min(from.X, to.X) + 1; j < max(from.X, to.X); j++ {
				c := from
				c.X = j
				brick = append(brick, c)
			}
		}
		
		if from.Y != to.Y {
			for j := min(from.Y, to.Y) + 1; j < max(from.Y, to.Y); j++ {
				c := from
				c.Y = j
				brick = append(brick, c)
			}
		}
		bricks = append(bricks, brick)
	}

	slices.SortFunc(bricks, func(b1, b2 brick) int {
		m := func(bs brick) int { return aoc.Min(aoc.Map(func(b cube) int { return b.Z }, bs)...) }
		return cmp(m(b1), m(b2))
	})

	stable := 0
	stack, _ := fall(bricks)
	for i := 0; i < len(stack); i++ {
		s := aoc.DeleteAt(aoc.Copy(stack), i)
		_, fell := fall(s)
		if fell == 0 {
			stable++
		}
	}
	return stable
}

func fall(bricks []brick) ([]brick, int) {
	supported := aoc.Set[cube]{}
	stack := []brick{}
	totalFell := 0
	for _, b := range bricks {
		fell := false
		for {
			falling := aoc.Map(func(c cube) cube { return cube{c.X, c.Y, c.Z - 1} }, b)
			if aoc.AnyFunc(func(c cube) bool { return c.Z == 0 || supported.Contains(c) }, falling...) {
				break
			} else {
				b = falling
				fell = true
			}
		}
		stack = append(stack, b)
		if fell {
			totalFell++
		}

		for _, c := range b {
			supported.Add(c)
		}
	}
	return stack, totalFell
}