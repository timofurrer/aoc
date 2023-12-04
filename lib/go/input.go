package aoc

import (
	"strconv"
	"strings"
)

// Int64 returns int64 from the given string or panics
func Int64(x string) int64 {
	i, err := strconv.ParseInt(x, 10, 64)
	if err != nil {
		panic("input is not a number")
	}

	return i
}

func ParseInt64List(s string) []int64{
	return Map(func(x string) int64 {
		return Int64(x)
	}, strings.Fields(s))
}