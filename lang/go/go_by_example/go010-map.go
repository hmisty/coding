package main

import "fmt"

func main() {

	m := make(map[string]int)

	m["k1"] = 7
	m["k2"] = 13

	fmt.Println("map:", m)

	v1, prs := m["k1"]
	fmt.Println("v1: ", v1, "prs:", prs)
	fmt.Println("len:", len(m))

	delete(m, "k2")
	fmt.Println("map:", m)

	v2, prs := m["k2"]
	fmt.Println("v2:", v2, "prs:", prs)

	n := map[string]int{"foo": 1, "bar": 2}
	fmt.Println("map:", n)

}
