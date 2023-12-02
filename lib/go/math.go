package aoc

import "golang.org/x/exp/constraints"

type Number interface {
	constraints.Integer | constraints.Float
}

func Sum[N Number](xs []N) N {
	return Reduce(func(acc, x N) N { return acc + x }, xs, *new(N))
}

// Min returns the minimum value in xs
// xs must be non-empty or it'll panic.
func Min[X constraints.Ordered](xs ...X) X {
	if len(xs) == 0 {
		panic("xs must be non-empty to get minimum value")
	}

	return Reduce(func(acc X, x X) X {
		if x < acc {
			return x
		} else {
			return acc
		}
	}, xs[1:], xs[0])
}

// Min returns the maximum value in xs
// xs must be non-empty or it'll panic.
func Max[X constraints.Ordered](xs ...X) X {
	if len(xs) == 0 {
		panic("xs must be non-empty to get maximum value")
	}

	return Reduce(func(acc X, x X) X {
		if x > acc {
			return x
		} else {
			return acc
		}
	}, xs[1:], xs[0])
}