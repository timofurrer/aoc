package aoc

func Map[X any, M any](f func(x X) M, xs []X) []M {
	mapped := make([]M, len(xs))
	for i, x := range xs {
		mapped[i] = f(x)
	}
	return mapped
}
