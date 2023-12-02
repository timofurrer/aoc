package aoc

func All[X bool](xs ...X) bool {
	for _, x := range xs {
		if !x {
			return false
		}
	}
	return true
}
