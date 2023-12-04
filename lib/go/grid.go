package aoc

import (
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

func AllNeighborCoords(c Point2d, n []Point2d) []Point2d {
	return Map(func(d Point2d) Point2d { return c + d }, n)
}

func DoEachNeighbor[X any](f func(n Point2d) X, c Point2d, n []Point2d) []X {
	return Map(func(d Point2d) X { return f(c + d) }, n)
}

func ManhattenDistance(a Point2d, b Point2d) int64 {
	d := a - b
	return Abs(int64(real(d))) + Abs(int64(imag(d)))
}

func EuclidianDistance(a Point2d, b Point2d) float64 {
	d := a - b
	return math.Sqrt(math.Pow(real(d), 2) + math.Pow(imag(d), 2))
}
