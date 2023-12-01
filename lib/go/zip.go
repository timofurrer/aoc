package aoc

func Zip[X any](xss ...[]X) [][]X {
	m := Min(Map(func(xs []X) int { return len(xs) }, xss)...)
	zs := make([][]X, 0, m)
	for i := 0; i < m; i++ {
		z := Map(func(xs []X) X { return xs[i] }, xss)
		zs = append(zs, z)
	}
	return zs
}
