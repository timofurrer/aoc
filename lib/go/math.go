package aoc

import "golang.org/x/exp/constraints"

type Number interface {
	constraints.Integer | constraints.Float
}

func Sum[N Number](xs []N) N {
	return Reduce(func(acc, x N) N { return acc + x }, xs, *new(N))
}
