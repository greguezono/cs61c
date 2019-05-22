package functions

import "testing"

func TestAbsSumPos(t *testing.T) {
	x := []int{12, 14, 16}
	sum := AbsSum(x)
	if sum != 42 {
		t.Error("Expected 42, got", sum)
	}
}

func TestAbsSumNeg(t *testing.T) {
	x := []int{-100, -50, 50, 0, -20, 20}
	sum := AbsSum(x)
	if sum != 100 {
		t.Error("Expected 100, got", sum)
	}
}

func TestProd(t *testing.T) {
	// Test our product function
	x := []int{-100, -50, 50, 0, -20, 20}
	prod := Prod(x)
	if prod != 0 {
		t.Error("Expected 0, got", prod)
	}
}

func TestSub(t *testing.T) {
	// Test our subtraction function
	x := []int{-100, -50, 50, 0, -20, 20}
	sum := Sub(x)
	if sum != 100 {
		t.Error("Expected 0, got", sum)
	}
}
