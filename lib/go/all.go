package aoc

func All[X bool](xs ...X) bool {
	for _, x := range xs {
		if !x {
			return false
		}
	}
	return true
}

func AllFunc[X any](f func(x X) bool, xs ...X) bool {
	for _, x := range xs {
		if !f(x) {
			return false
		}
	}
	return true
}