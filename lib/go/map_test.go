package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMap_Int(t *testing.T) {
	xs := []int{2, 4, 6}
	m := Map(func(x int) int { return x * 2 }, xs)
	require.Equal(t, []int{4, 8, 12}, m)
}

func TestMap_String(t *testing.T) {
	xs := []string{"a", "b", "c"}
	m := Map(func(x string) string { return x + x }, xs)
	require.Equal(t, []string{"aa", "bb", "cc"}, m)
}

func TestMap_StringToInt(t *testing.T) {
	xs := []string{"a", "b", "c"}
	m := Map(func(x string) rune { return rune(x[0]) }, xs)
	require.Equal(t, []rune{'a', 'b', 'c'}, m)
}
