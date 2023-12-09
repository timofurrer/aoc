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

func AllEq[X comparable](xs ...X) bool {
	for i := 0; i < len(xs)-1; i++ {
		if xs[i] != xs[i+1] {
			return false
		}
	}
	return true
}