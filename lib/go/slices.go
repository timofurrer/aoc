package aoc

func Last[X any](xs []X) X {
	return xs[len(xs) - 1]
}