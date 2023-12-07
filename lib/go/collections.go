package aoc

import (
	"slices"

	"golang.org/x/exp/maps"
)

type Counter[A comparable] map[A]int

func NewCounter[A comparable](xs []A) Counter[A] {
	c := Counter[A]{}
	for _, r := range xs {
		c[r] += 1
	}
	return c
}

func (c Counter[A]) Values() []int {
	v := maps.Values(c)
	slices.Sort(v)
	return v
}