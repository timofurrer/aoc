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

// Int64 returns int64 from the given string or panics
func Int(x string) int {
	i, err := strconv.ParseInt(x, 10, 64)
	if err != nil {
		panic(fmt.Errorf("input is not a number: %q", x))
	}

	return int(i)
}

func IntFromHex(x string) int {
	i, err := strconv.ParseInt(x, 16, 64)
	if err != nil {
		panic(fmt.Errorf("input is not a number: %q", x))
	}

	return int(i)
}

func ParseInt64List(s string) []int64{
	return Map(func(x string) int64 {
		return Int64(x)
	}, strings.Fields(s))
}