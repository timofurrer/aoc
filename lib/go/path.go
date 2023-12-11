package aoc

func EnclosedAreaWithinPath(path []Point2d) int64 {
	pathLen := len(path)
	var area int64
	for i, cur := range path {
		next := path[(i + 1) % pathLen]
		area += Area(cur, next)
	}

	return Abs(area) / 2 - int64(pathLen) / 2 + 1
}