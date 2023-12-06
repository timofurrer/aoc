package aoc

import (
	"fmt"
	"strconv"
	"strings"
)

// Int64 returns int64 from the given string or panics
func Int64(x string) int64 {
	i, err := strconv.ParseInt(x, 10, 64)
	if err != nil {
		panic(fmt.Errorf("input is not a number: %q", x))
	}

	return i
}

func ParseInt64List(s string) []int64{
	return Map(func(x string) int64 {
		return Int64(x)
	}, strings.Fields(s))
}