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

type hailstone struct {
	x, y, z float64
	vx, vy, vz float64
}

const (
	// minIntersection = 7
	// maxIntersection = 27
	minIntersection = 200000000000000
	maxIntersection = 400000000000000
)

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	hailstones := []hailstone{}
	for scanner.Scan() {
		line := scanner.Text()

		parts := strings.Split(line, " @ ")
		position := strings.Split(parts[0], ", ")
		velocity := strings.Split(parts[1], ", ")
		h := hailstone{
			x: float64(aoc.Int(strings.TrimSpace(position[0]))),
			y: float64(aoc.Int(strings.TrimSpace(position[1]))),
			z: float64(aoc.Int(strings.TrimSpace(position[2]))),
			vx: float64(aoc.Int(strings.TrimSpace(velocity[0]))),
			vy: float64(aoc.Int(strings.TrimSpace(velocity[1]))),
			vz: float64(aoc.Int(strings.TrimSpace(velocity[2]))),
		}
		hailstones = append(hailstones, h)
	}

	intersect := 0
	for i := 0; i < len(hailstones)-1; i++ {
		for j := i + 1; j < len(hailstones); j++ {
			x, y, err := findIntersection(hailstones[i], hailstones[j])

			if err == nil && x >= minIntersection && x <= maxIntersection && y >= minIntersection && y <= maxIntersection {
				intersect++
			}
		}
	}


	return intersect
}

func findIntersection(h1, h2 hailstone) (float64, float64, error) {
	m1 := h1.vy / h1.vx
	m2 := h2.vy / h2.vx
	if m1 == m2 {
		return 0, 0, fmt.Errorf("parallel")
	}

	b1 := h1.y - m1*h1.x
	b2 := h2.y - m2*h2.x
	x := (b2-b1) / (m1-m2)
	y := m1*x + b1

	if !((x > h1.x && h1.vx > 0) || (x < h1.x && h1.vx < 0)) || !((x > h2.x && h2.vx > 0) || (x < h2.x && h2.vx < 0)) {
		return 0, 0, fmt.Errorf("in the future")
	}

	return x, y, nil
}