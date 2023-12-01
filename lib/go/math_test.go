package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMin(t *testing.T) {
	xs := []int{5, 6, 9, 10, 2}
	m := Min(xs...)
	require.Equal(t, 2, m)
}

func TestMax(t *testing.T) {
	xs := []int{5, 6, 9, 10, 2}
	m := Max(xs...)
	require.Equal(t, 10, m)
}
