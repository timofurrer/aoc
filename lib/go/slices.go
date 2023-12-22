package aoc

import "golang.org/x/exp/slices"

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

func Copy[X any](slice []X) []X {
    tmp := make([]X, len(slice))
	copy(tmp, slice)
    return tmp
}

func Reverse[X any](slice []X) []X {
    tmp := Copy(slice)
    slices.Reverse(tmp)
    return tmp
}

func RotateCounterClockwise[X any](slice [][]X) [][]X {
    lenX := len(slice[0])
    lenY := len(slice)
	rot := make([][]X, lenX)
	for x := range rot {
		rot[x] = make([]X, lenY)
	}

	for y := 0; y < lenY; y++ {
		for x := 0; x < lenX; x++ {
			rot[x][lenY - y - 1] = slice[y][x]
		}
	}
	return rot
}

func Count[X comparable](slice []X, needle X) int {
    return len(Filter(func(x X) bool { return x == needle }, slice))
}

func Delete[X comparable](slice []X, el X) []X {
    idx := Find(slice, el)
    if idx > -1 {
        return slices.Delete(slice, idx, idx+1)
    }
    return slice
}

func Find[X comparable](slice []X, el X) int {
    for i := range slice {
        if slice[i] == el {
            return i
        }
    }
    return -1
}

func DeleteAt[X any](slice []X, s int) []X {
    return append(slice[:s], slice[s+1:]...)
}