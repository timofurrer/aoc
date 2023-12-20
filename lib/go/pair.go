package aoc

// Pair is a dead-simple 2-value pair.
type Pair[A any, B any] struct {
	A A
	B B
}

func NewPair[A any, B any](a A, b B) Pair[A, B] {
	return Pair[A, B]{A: a, B: b}
}

type Triple[A any, B any, C any] struct {
	A A
	B B
	C C
}

func NewTriple[A any, B any, C any](a A, b B, c C) Triple[A, B, C] {
	return Triple[A, B, C]{A: a, B: b, C: c}
}