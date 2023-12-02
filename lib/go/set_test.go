package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)
func TestSetUnion(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(4, 2, 5)

	diff := a.Union(b)

	require.Equal(t, NewSet(1, 2, 3, 4, 5), diff)
}

func TestSetDifference(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(4, 2, 5)

	diff := a.Difference(b)

	require.Equal(t, NewSet(1, 3), diff)
}

func TestSetSymmetricDifference(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(4, 2, 5)

	diff := a.SymmetricDifference(b)

	require.Equal(t, NewSet(1, 3, 4, 5), diff)
}
