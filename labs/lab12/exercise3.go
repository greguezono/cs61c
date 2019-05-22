package main

import "fmt"

func return_range(n int, c chan int) {
	// Implement Me
	for i := 1; i < n+1; i++ {
		c <- i
	}
	close(c)
}

func main() {
	size := 10
	sum := 0
	// Implement Me
	c := make(chan int, size)
	go return_range(cap(c), c)
	for i := range c {
		sum = sum + (i*i + 1)
	}
	fmt.Println(sum)

}
