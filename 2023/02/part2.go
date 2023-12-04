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

type reveal struct {
	red, green, blue int64
}

func solve(input io.Reader) int64 {
	scanner := bufio.NewScanner(input)

	mins := []int64{}
	gameId := 0
	for scanner.Scan() {
		gameId++
		line := scanner.Text()
		p := strings.Split(line, ":")
		rawReveals := p[1]
		reveals := []reveal{}
		for _, cubes := range strings.Split(rawReveals, ";") {
			r := reveal{}
			for _, cube := range strings.Split(cubes, ",") {
				cubeAmount := strings.Fields(cube)
				switch cubeAmount[1] {
				case "red":
					r.red = aoc.Int64(cubeAmount[0])
				case "green":
					r.green = aoc.Int64(cubeAmount[0])
				case "blue":
					r.blue = aoc.Int64(cubeAmount[0])
				}
			}

			reveals = append(reveals, r)
		}

		minReveal := aoc.Reduce(func(acc reveal, x reveal) reveal {
			return reveal{
				red:   aoc.Max(acc.red, x.red),
				green: aoc.Max(acc.green, x.green),
				blue:  aoc.Max(acc.blue, x.blue),
			}
		}, reveals[1:], reveals[0])

		mins = append(mins, minReveal.red*minReveal.green*minReveal.blue)
	}

	return aoc.Sum(mins)
}
