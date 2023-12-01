package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestReduce_Int(t *testing.T) {
	xs := []int{5, 10, 15}
	x := Reduce(func(acc, x int) int { return acc + x }, xs, 0)

	require.Equal(t, 30, x)
}

func TestReduce_String(t *testing.T) {
	xs := []string{"a", "b", "c"}
	x := Reduce(func(acc, x string) string { return acc + x }, xs, "")

	require.Equal(t, "abc", x)
}
