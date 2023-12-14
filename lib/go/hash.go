package aoc

import (
	"fmt"

	"github.com/mitchellh/hashstructure/v2"
)

type Hashed uint64

func Hash[X any](x X) Hashed {
	h, err := hashstructure.Hash(x, hashstructure.FormatV2, nil)
	if err != nil {
		panic(fmt.Errorf("unable to get hash from %+v: %w", x, err))
	}
	return Hashed(h)
}