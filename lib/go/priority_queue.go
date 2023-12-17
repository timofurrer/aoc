// This example demonstrates a priority queue built using the heap interface.
package aoc

import (
	"container/heap"
)

// An Item is something we manage in a priority queue.
type Item[X any] struct {
	Value    X // The value of the item; arbitrary.
	Priority int    // The priority of the item in the queue.
	// The index is needed by update and is maintained by the heap.Interface methods.
	index int // The index of the item in the heap.
}

// A PriorityQueue implements heap.Interface and holds Items.
type PriorityQueue[X any] []*Item[X]

func (pq PriorityQueue[X]) Len() int { return len(pq) }

func (pq PriorityQueue[X]) Less(i, j int) bool {
	return pq[i].Priority < pq[j].Priority
}

func (pq PriorityQueue[X]) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue[X]) Push(x any) {
	n := len(*pq)
	item := x.(*Item[X])
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue[X]) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid memory leak
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

// update modifies the priority and value of an Item in the queue.
func (pq *PriorityQueue[X]) update(item *Item[X], value X, priority int) {
	item.Value = value
	item.Priority = priority
	heap.Fix(pq, item.index)
}