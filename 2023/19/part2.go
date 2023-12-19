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

type workflow struct {
	cs []condition
}

type condition struct {
	p  byte
	op byte
	v  int64
	t string
}

type ranges map[byte]aoc.Pair[int64, int64]

func (r ranges) sum() int64 {
	rs := func(c byte) int64 { x := r[c]; return x.B - x.A + 1 }
	return rs('x') * rs('m') * rs('a') * rs('s')
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	workflows := map[string]workflow{}
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			break
		}

		cIdx := strings.Index(line, "{")
		name := line[:cIdx]

		w := workflow{}
		rawConditions := strings.Split(strings.TrimRight(line, "}")[cIdx+1:], ",")
		for _, rc := range rawConditions {
			if cond, target, found := strings.Cut(rc, ":"); found {
				w.cs = append(w.cs, condition{
					p:  cond[0],
					op: cond[1],
					v:  aoc.Int64(cond[2:]),
					t: target,
				})
			} else {
				w.cs = append(w.cs, condition{op: 0, t: rc})
			}
		}

		workflows[name] = w
	}

	s := getRanges("in", workflows, ranges{
		'x': aoc.NewPair[int64, int64](1, 4000),
		'm': aoc.NewPair[int64, int64](1, 4000),
		'a': aoc.NewPair[int64, int64](1, 4000),
		's': aoc.NewPair[int64, int64](1, 4000),
	})
	return s
}

func getRanges(current string, workflows map[string]workflow, r ranges) int64 {
	if current == "R" {
		return 0
	}
	if current == "A" {
		return r.sum()
	}

	var sum int64
	for _, cs := range workflows[current].cs {
		if cs.op == 0 {
			sum += getRanges(cs.t, workflows, r)
		} else {
			nr := make(ranges, len(r))
			maps.Copy(nr, r)

			cr := r[cs.p]
			if cr.A < cs.v && cs.v < cr.B {
				switch cs.op {
				case '<':
					nr[cs.p] = aoc.NewPair(cr.A, cs.v - 1)
					r[cs.p] = aoc.NewPair(cs.v, cr.B)
				case '>':
					nr[cs.p] = aoc.NewPair(cs.v + 1, cr.B)
					r[cs.p] = aoc.NewPair(cr.A, cs.v)
				}
				sum += getRanges(cs.t, workflows, nr)
			}
		}
	}
	return sum
}