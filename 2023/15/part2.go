package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
)


func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)
 
	hashmap := make([][]aoc.Pair[string, int], 256)
	for scanner.Scan() {
		line := scanner.Text()
		ds := strings.Split(line, ",")
		
		for _, instr := range ds {
			if label, focalLengthStr, found := strings.Cut(instr, "="); found {
				focalLength := aoc.Int(focalLengthStr)
				h := hash(label)
				box := hashmap[h]
				if len(aoc.Filter(func(p aoc.Pair[string, int]) bool { return p.A == label}, box)) == 0 {
					hashmap[h] = append(box, aoc.NewPair(label, focalLength))
				} else {
					for i := range box {
						if box[i].A == label {
							box[i].B = focalLength
						}
					}
				}
			} 
			if label, found := strings.CutSuffix(instr, "-"); found {
				h := hash(label)
				for i, p := range hashmap[h] {
					if p.A == label {
						hashmap[h] = slices.Delete(hashmap[h], i, i+1)
					}
				}
			}
		}
	}


	s := 0
	for i, box := range hashmap {
		for j, lens := range box {
			s += (i + 1) * (j + 1) * lens.B
		}
	}
	return s
}

func hash(data string) int {
	return aoc.Reduce(func(acc int, x rune) int { return ((acc + int(x)) * 17) % 256 }, []rune(data), 0)
}