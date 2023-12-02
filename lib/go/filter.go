package aoc

func Filter[X any](filter func(x X) bool, xs []X) []X {
	filtered := make([]X, 0, len(xs))
	for _, x := range xs {
		if filter(x) {
			filtered = append(filtered, x)
		}
	}
	return filtered
}

func FilterMap[X any, M any](f func(x X) (M, bool), xs []X) []M {
	res := make([]M, 0, len(xs))
	for _, x := range xs {
		if m, ok := f(x); ok {
			res = append(res, m)
		}
	}
	return res
}