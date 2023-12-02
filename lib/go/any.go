package aoc

func Any[X bool](xs ...X) bool {
	for _, x := range xs {
		if x {
			return true
		}
	}
	return false
}
