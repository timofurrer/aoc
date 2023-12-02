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

// Intersection returns the elemtsn that are common in s and other
func (s Set[X]) Intersection(other Set[X]) Set[X] {
	intersection := make(Set[X])
	for x := range s {
		if v, ok := other[x]; ok {
			intersection[x] = v
		}
	}
	return intersection
}

// Difference returns the elements that are in s, but not in other
func (s Set[X]) Difference(other Set[X]) Set[X] {
	diff := make(Set[X])
	for x := range s {
		if v, ok := other[x]; !ok {
			diff[x] = v
		}
	}

	return diff
}

// SymmetricDifference returns the elements that are either in s or other, but not in both
func (s Set[X]) SymmetricDifference(other Set[X]) Set[X] {
	diff := make(Set[X])
	for x := range s {
		if v, ok := other[x]; !ok {
			diff[x] = v
		}
	}
	for x := range other {
		if v, ok := s[x]; !ok {
			diff[x] = v
		}
	}
	return diff
}

// IsSubSet returns wether s is a subset of other
func (s Set[X]) IsSubSet(other Set[X]) bool {
	for x := range s {
		if _, ok := other[x]; !ok {
			return false
		}
	}
	return true
}

// IsSuperSet returns wether s is a superset of other
func (s Set[X]) IsSuperSet(other Set[X]) bool {
	for x := range other {
		if _, ok := s[x]; !ok {
			return false
		}
	}
	return true
}

// IsDisjoint returns true if no element of s are in other
func (s Set[X]) IsDisjoint(other Set[X]) bool {
	for x := range s {
		if _, ok := other[x]; ok {
			return false
		}
	}
	return true
}
