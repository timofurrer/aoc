package aoc

func Any[X bool](xs ...X) bool {
	for _, x := range xs {
		if x {
			return true
		}
	}
	return false
}

func AnyFunc[X any](f func(x X) bool, xs ...X) bool {
	for _, x := range xs {
		if f(x) {
			return true
		}
	}
	return false
}