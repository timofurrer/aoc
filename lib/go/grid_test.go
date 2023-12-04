package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGridAllNeighborCoords(t *testing.T) {
	c := Point2d(5 + 42i)

	neighbors := AllNeighborCoords(c, Neighbors2d4)

	require.Equal(t, []Point2d{
		5 + 43i,
		5 + 41i,
		6 + 42i,
		4 + 42i,
	}, neighbors)
}

func TestGridDoEachNeighbor(t *testing.T) {
	c := Point2d(5 + 42i)

	neighbors := DoEachNeighbor(func(n Point2d) float64 {
		return real(n) + imag(n)
	}, c, Neighbors2d4)

	require.Equal(t, []float64{48, 46, 48, 46}, neighbors)
}