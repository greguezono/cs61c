// Put package name here
package main

// Put imported packages here
import (
	"fmt"
	"math/rand"
)

func main() {
	// Put our function calls here
	fmt.Println("Hello World!")
	random()
}

func random() {
	fmt.Println("You're random number of the day is", rand.Intn(10))
}

// Put Random function here
