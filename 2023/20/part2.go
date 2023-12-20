package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	aoc "github.com/timofurrer/aoc/lib/go"
	"golang.org/x/exp/maps"
)

func main() {
	input := os.Stdin

	answer := solve(input)

	fmt.Printf("Answer: %d\n", answer)
}

type pulse int

type module interface {
	ConnectTo(dst module)
	GetName() string
	Send(src string, input pulse) []aoc.Pair[module, pulse]
}

type broadcaster struct {
	name string
	dsts []module
}

type flipflop struct {
	name  string
	dsts  []module
	onOff bool
}

type conjunction struct {
	name        string
	dsts        []module
	inputStates map[string]pulse

	pulseToProcess *pulse
}

func (b *broadcaster) ConnectTo(dst module) {
	b.dsts = append(b.dsts, dst)
}

func (b *broadcaster) GetName() string {
	return b.name
}

func (b *broadcaster) Send(src string, input pulse)  []aoc.Pair[module, pulse]{
	return aoc.Map(func(d module) aoc.Pair[module, pulse] { return aoc.NewPair(d, input) }, b.dsts)
}

func (b *broadcaster) Process() {}

func (f *flipflop) Send(src string, input pulse)  []aoc.Pair[module, pulse] {
	if input == 1 {
		return nil
	}
	if !f.onOff {
		f.onOff = true
		return aoc.Map(func(d module) aoc.Pair[module, pulse] { return aoc.NewPair(d, pulse(1)) }, f.dsts)
	} else {
		f.onOff = false
		return aoc.Map(func(d module) aoc.Pair[module, pulse] { return aoc.NewPair(d, pulse(0)) }, f.dsts)
	}
}

func (f *flipflop) ConnectTo(dst module) {
	f.dsts = append(f.dsts, dst)
}

func (f *flipflop) GetName() string {
	return f.name
}

func (c *conjunction) Send(src string, input pulse)  []aoc.Pair[module, pulse] {
	c.inputStates[src] = input
	if aoc.AllFunc(func(p pulse) bool { return p == 1 }, maps.Values(c.inputStates)...) {
		return aoc.Map(func(d module) aoc.Pair[module, pulse] { return aoc.NewPair(d, pulse(0)) }, c.dsts)
	} else {
		return aoc.Map(func(d module) aoc.Pair[module, pulse] { return aoc.NewPair(d, pulse(1)) }, c.dsts)
	}
}

func (c *conjunction) RegisterInput(src string) {
	c.inputStates[src] = 0
}

func (c *conjunction) ConnectTo(dst module) {
	c.dsts = append(c.dsts, dst)
}

func (c *conjunction) GetName() string {
	return c.name
}

func solve(input io.Reader) int {
	scanner := bufio.NewScanner(input)

	modules := map[string]module{}
	cables := map[string][]string{}
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " -> ")
		var m module
		switch parts[0][0] {
		case 'b':
			m = &broadcaster{name: parts[0]}
		case '%':
			m = &flipflop{name: parts[0][1:]}
		case '&':
			m = &conjunction{name: parts[0][1:], inputStates: make(map[string]pulse)}
		}
		modules[m.GetName()] = m
		dsts := strings.Split(parts[1], ", ")
		cables[m.GetName()] = dsts
	}

	for m, c := range cables {
		for _, d := range c {
			if dst, ok := modules[d]; ok {
				modules[m].ConnectTo(dst)

				switch c := dst.(type) {
				case *conjunction:
					c.RegisterInput(m)
				}
			} else {
				modules[m].ConnectTo(nil)
			}
		}
	}

	//
	// Analyzing Input manually:
	//
	// rx is receiving pulse from a single conjunction at L46: &vf -> rx
	//
	// vf is receiving pulse from:
	// - L10: &pm -> vf
	// - L32: &mk -> vf
	// - L39: &pk -> vf 
	// - L54: &hf -> vf
	//
	// Those must all be flipping to high so that vf (a conjunction) sends
	// a low pulse.

	highs := []int{}
	var pushes int
	for pushes = 1; len(highs) < 4; pushes++ {
		for _ = range pushRx(modules) {
			highs = append(highs, pushes)
		}
	}
	return aoc.LCM(highs...)
}

func pushRx(modules map[string]module) []string {
	lo := 0
	hi := 0
	frontier := []aoc.Triple[string, module, pulse]{{"button", modules["broadcaster"], 0}}
	highs := []string{}
	for len(frontier) > 0 {
		current := frontier[0]
		frontier = frontier[1:]

		if current.C == 0 {
			lo++
		} else {
			hi++
		}

		if current.B != nil {
			if current.B.GetName() == "vf" && current.C == 1 {
				highs = append(highs, current.A)
			}

			dsts := current.B.Send(current.A, current.C)
			frontier = append(frontier, aoc.Map(func(d aoc.Pair[module, pulse]) aoc.Triple[string, module, pulse] {
				return aoc.NewTriple(current.B.GetName(), d.A, d.B)
			}, dsts)...)
		} 
	}
	return highs
}