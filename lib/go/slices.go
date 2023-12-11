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