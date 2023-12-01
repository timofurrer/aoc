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

