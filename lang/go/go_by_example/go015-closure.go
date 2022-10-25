package main

import "fmt"

func intSeq() func() int {
	i := 0
	return func() int {
		i += 1
		return i
	}
}

func main() {
	nextInt := intSeq()

	for i := 0; i < 5; i++ {
		fmt.Println(nextInt())
	}

	newInt := intSeq()
	fmt.Println(newInt())
}
