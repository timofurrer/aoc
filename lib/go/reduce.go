package aoc

func Reduce[X any, A any](reducer func(acc A, x X) A, xs []X, init A) A {
	acc := init
	for _, x := range xs {
		acc = reducer(acc, x)
	}
	return acc
}

func ReduceReverse[X any, A any](reducer func(acc A, x X) A, xs []X, init A) A {
	acc := init
	for i := len(xs) - 1; i >= 0; i-- {
		acc = reducer(acc, xs[i])
	}
	return acc
}

