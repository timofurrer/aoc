package aoc

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

var (
	Neighbors2d4 = []Point2d{
		0 + 1i,
		0 + -1i,
		1 + 0i,
		-1 + 0i,
	}
	Neighbors2d8 = []Point2d{
		0 + 1i,
		0 + -1i,
		1 + 0i,
		-1 + 0i,
		1 + 1i,
		1 + -1i,
		-1 + 1i,
		-1 + -1i,
	}
)

// Point2d is just a very simple wrapper around a complex128 
// The x, y coordinates are stored in the real and imag parts of the complex
// number. The complex number is used to easily have comparable types
// and easily support properties like addition and substraction of points.
type Point2d complex128

func NewPoint2d[X Number](x, y X) Point2d {
	return Point2d(complex(float64(x), float64(y)))
}

func (p Point2d) X() int64 {
	return int64(real(p))
}

func (p Point2d) Y() int64 {
	return int64(imag(p))
}

func AllNeighborCoords(c Point2d, ns []Point2d) []Point2d {
	return Map(func(d Point2d) Point2d { return c + d }, ns)
}

func DoEachNeighbor[X any](f func(n Point2d) X, c Point2d, ns []Point2d) []X {
	return Map(func(d Point2d) X { return f(c + d) }, ns)
}

func ManhattenDistance(a Point2d, b Point2d) int64 {
	d := a - b
	return Abs(int64(real(d))) + Abs(int64(imag(d)))
}

func EuclidianDistance(a Point2d, b Point2d) float64 {
	d := a - b
	return math.Sqrt(math.Pow(real(d), 2) + math.Pow(imag(d), 2))
}

func Area(a Point2d, b Point2d) int64 {
	return a.X() * b.Y() - a.Y() * b.X()
}

type Grid2d[X comparable, Y comparable] map[X]Y

func NewGrid2d[X comparable, Y comparable]() Grid2d[X, Y] {
	return make(Grid2d[X, Y])
}

func ParseGrid2d[Y comparable](input io.Reader, v func(x rune) Y) Grid2d[Point2d, Y] {
	g := NewGrid2d[Point2d, Y]()
	scanner := bufio.NewScanner(input)
	for row := 0; scanner.Scan(); row++ {
		line := scanner.Text()
		for col, p := range line {
			g.Add(NewPoint2d(col, row), v(p))
		}
	}
	return g
}

func (g Grid2d[X, Y]) Add(p X, v Y) {
	g[p] = v
}

func (g Grid2d[X, Y]) Find(v Y) (X, bool) {
	for p, vP := range g {
		if vP == v {
			return p, true
		}
	}
	return *new(X), false
}

func (g Grid2d[X, Y]) FindOrPanic(v Y) X {
	if p, found := g.Find(v); found {
		return p
	}
	panic(fmt.Errorf("unable to find %+v in grid", v))
}

func (g Grid2d[X, Y]) Walk(start X, end X, nextF func(cur X, prev X) X) []X {
	var prev X
	cur := start

	path := []X{}
	for cur != start || len(path) == 0 {
		next := nextF(cur, prev)
		path = append(path, cur)
		prev = cur
		cur = next
	}
	return path
}