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

type workflowF struct {
	conditions []func(p part) string
}

type part struct {
	c string
	p map[byte]int64
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)
 
	workflows := map[string]workflowF{
		"A": workflowF{},
		"R": workflowF{},
	}
	parts := []*part{}
	parsingWorkflows := true
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			parsingWorkflows = false
			continue
		}
		
		if parsingWorkflows {
			cIdx := strings.Index(line, "{")
			name := line[:cIdx]

			w := workflowF{}
			rawConditions := strings.Split(strings.TrimRight(line, "}")[cIdx + 1:], ",")
			for _, rc := range rawConditions {
				if cond, target, found := strings.Cut(rc, ":"); found {
					w.conditions = append(w.conditions, func(p part) string {
						a := p.p[cond[0]]
						op := cond[1]
						param := aoc.Int64(cond[2:])

						switch op {
						case '<':
							if a < param {
								return target
							}
						case '>':
							if a > param {
								return target
							}
						}
						return ""
					})
				} else {
					w.conditions = append(w.conditions, func(p part) string { return rc }) 
				}
			}

			workflows[name] = w
		} else {
			params := strings.Split(strings.Trim(line, "{}"), ",")
			p := &part{c: "in", p: make(map[byte]int64, len(params))}
			for _, param := range params {
				a := param[0]
				v := aoc.Int64(param[2:])
				p.p[a] = v
			}
			parts = append(parts, p)
		}
	}

	for _, part := range parts {
		for part.c != "A" && part.c != "R" {
			w := workflows[part.c]
			for _, cond := range w.conditions {
				if n := cond(*part); n != "" {
					part.c = n
					break	
				}
			}
		}
	}

	accepted := aoc.Filter(func(p *part) bool { return p.c == "A" }, parts)
	s := aoc.SumMap(func(p *part) int64 { return aoc.Sum(maps.Values(p.p)) }, accepted)
	return s
}
