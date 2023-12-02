package aoc

type Set[X comparable] map[X]struct{}

func NewSet[X comparable](xs ...X) Set[X] {
	s := make(Set[X], len(xs))
	for _, x := range xs {
		s[x] = struct{}{}
	}
	return s
}

func (s Set[X]) Contains(x X) bool {
	_, ok := s[x]
	return ok
}

// Union returns the elements of both s and other combined
func (s Set[X]) Union(other Set[X]) Set[X] {
	union := make(Set[X], len(s))
	for x, y := range s {
		union[x] = y
	}
	for x, y := range other {
		union[x] = y
	}
	return union
}

// Difference returns the elements that are in s, but not in other
func (s Set[X]) Difference(other Set[X]) Set[X] {
	diff := make(Set[X])
	for x := range s {
		if _, ok := other[x]; !ok {
			diff[x] = struct{}{}
		}
	}

	return diff
}

// SymmetricDifference returns the elements that are either in s or other, but not in both
func (s Set[X]) SymmetricDifference(other Set[X]) Set[X] {
	diff := make(Set[X])
	for x := range s {
		if _, ok := other[x]; !ok {
			diff[x] = struct{}{}
		}
	}
	for x := range other {
		if _, ok := s[x]; !ok {
			diff[x] = struct{}{}
		}
	}
	return diff
}
