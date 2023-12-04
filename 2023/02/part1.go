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

const (
	maxRed   = 12
	maxGreen = 13
	maxBlue  = 14
)

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	possibleGameIds := []int{}
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

		if aoc.AllFunc(func(x reveal) bool {
			return x.red <= maxRed && x.green <= maxGreen && x.blue <= maxBlue
		}, reveals...) {
			possibleGameIds = append(possibleGameIds, gameId)
		}
	}

	return aoc.Sum(possibleGameIds)
}
