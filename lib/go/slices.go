package aoc

func Last[X any](xs []X) X {
	return xs[len(xs) - 1]
}

func Transpose[X any](slice [][]X) [][]X {
    xl := len(slice[0])
    yl := len(slice)
    result := make([][]X, xl)
    for i := range result {
        result[i] = make([]X, yl)
    }
    for i := 0; i < xl; i++ {
        for j := 0; j < yl; j++ {
            result[i][j] = slice[j][i]
        }
    }
    return result
}

func Repeat[X any](slice []X, n int) []X {
    l := make([]X, 0, len(slice) * n)
    for i := 0; i < n; i++ {
        l = append(l, slice...)
    }
    return l
}

func RepeatWithSeparator[X any](slice []X, n int, sep X) []X {
    l := make([]X, 0, len(slice) * n + n - 1)
    l = append(l, slice...)
    for i := 0; i < n - 1; i++ {
        l = append(l, sep)
        l = append(l, slice...)
    }
    return l
}