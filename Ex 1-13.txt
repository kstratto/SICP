#lang racket
; Using induction to prove that the nth Fibonacci number is equal to
; (phi^n - psi^n)/sqrt(5), where phi = (1 + sqrt(5))/2 and psi = (1 - sqrt(5))/2
; is very straightforward. Prove the base cases n = 0, 1, and then use the
; definition of the Fibonacci sequence Fib(n) = Fib(n - 1) + Fib(n - 2)
; in the induction step. The key is to prove that phi^(n-1) + phi^(n-2) = phi^n
; and that psi^(n-1) + psi^(n-2) = psi^n. This will involve factoring out
; the n-1 power and then using the definitions of phi and psi.