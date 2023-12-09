package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestAllEq_false_odd(t *testing.T) {
	xs := []int{1, 2, 3}
	eq := AllEq(xs...)
	require.False(t, eq)
}

func TestAllEq_false_even(t *testing.T) {
	xs := []int{1, 2, 3, 3}
	eq := AllEq(xs...)
	require.False(t, eq)
}

func TestAllEq_true_odd(t *testing.T) {
	xs := []int{1, 1, 1}
	eq := AllEq(xs...)
	require.True(t, eq)
}

func TestAllEq_true_even(t *testing.T) {
	xs := []int{1, 1, 1, 1}
	eq := AllEq(xs...)
	require.True(t, eq)
}