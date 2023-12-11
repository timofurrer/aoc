package aoc

func DistinctValueCombinations[X comparable](xs []X) []Pair[X, X] {
	result := Set[Pair[X, X]]{}
	for _, a := range xs {
		for _, b := range xs {
			if a == b {
				continue
			}

			if !result.Contains(NewPair(b, a)) {
				result.Add(NewPair(a, b))
			}
		}
	}
	return result.ToSlice()
}