package aoc

import "golang.org/x/exp/constraints"

type Number interface {
	constraints.Integer | constraints.Float
}

func Sum[N Number](xs []N) N {
	return Reduce(func(acc, x N) N { return acc + x }, xs, *new(N))
}

func SumFunc[N Number, Y any](f func (acc N, y Y) N, xs []Y) N {
	return Reduce(func(acc N, x Y) N { return f(acc, x) }, xs, *new(N))
}

func SumMap[N Number, X any](f func(x X) N, xs []X) N {
	return Reduce(func(acc N, x X) N { return acc + f(x) }, xs, *new(N))
}

func Mul[N Number](xs []N) N {
	switch len(xs) {
	case 0:
		return *new(N)
	case 1:
		return xs[0]
	default:
		return Reduce(func(acc, x N) N { return acc * x }, xs[1:], xs[0])
	}
}

// Min returns the minimum value in xs
// xs must be non-empty or it'll panic.
func Min[X constraints.Ordered](xs ...X) X {
	if len(xs) == 0 {
		panic("xs must be non-empty to get minimum value")
	}

	return Reduce(func(acc X, x X) X {
		if x < acc {
			return x
		} else {
			return acc
		}
	}, xs[1:], xs[0])
}

// Min returns the maximum value in xs
// xs must be non-empty or it'll panic.
func Max[X constraints.Ordered](xs ...X) X {
	if len(xs) == 0 {
		panic("xs must be non-empty to get maximum value")
	}

	return Reduce(func(acc X, x X) X {
		if x > acc {
			return x
		} else {
			return acc
		}
	}, xs[1:], xs[0])
}

func MaxWithKey[X any, Y constraints.Ordered](key func(X) Y, xs ...X) X {
	if len(xs) == 0 {
		panic("xs must be non-empty to get maximum value")
	}

	m := Reduce(func(acc Pair[X, Y], x X) Pair[X, Y] {
		if k := key(x); k > acc.B {
			return NewPair(x, k)
		} else {
			return acc
		}
	}, xs[1:], NewPair(xs[0], key(xs[0])))

	return m.A
}

func Abs[X constraints.Integer](x X) X {
	if x < 0 {
		return -x
	}
	return x
}

func LCM[X constraints.Integer](xs ...X) X {
	r := X(1)
	for _, x := range xs {
		r = r * x / GCD(r, x)
	}
	return r
}

func GCD[X constraints.Integer](a, b X) X {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}