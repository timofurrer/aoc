package aoc

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestSetUnion(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(4, 2, 5)

	union := a.Union(b)

	require.Equal(t, NewSet(1, 2, 3, 4, 5), union)
}

func TestSetIntersection(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(4, 2, 5)

	intersection := a.Intersection(b)

	require.Equal(t, NewSet(2), intersection)
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

func TestSetIsSubSet_True(t *testing.T) {
	a := NewSet(1, 2)
	b := NewSet(1, 2, 4, 5)

	isSubSet := a.IsSubSet(b)

	require.True(t, isSubSet)
}

func TestSetIsSubSet_False(t *testing.T) {
	a := NewSet(1, 2)
	b := NewSet(1, 4, 5)

	isSubSet := a.IsSubSet(b)

	require.False(t, isSubSet)
}

func TestSetIsSuperSet_True(t *testing.T) {
	a := NewSet(1, 2, 3)
	b := NewSet(1, 2)

	isSuperSet := a.IsSuperSet(b)

	require.True(t, isSuperSet)
}


func TestSetIsSuperSet_False(t *testing.T) {
	a := NewSet(1)
	b := NewSet(1, 2)

	isSuperSet := a.IsSuperSet(b)

	require.False(t, isSuperSet)
}
