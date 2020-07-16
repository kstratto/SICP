#lang racket
; Since Louis's version of expmod uses explicit multiplication in the reduction case
; when the exponent b instead of calling square, applicative order evaluation requires
; (expmod base (/ exp 2) m) to be computed twice instead of once. The use of the square
; procedure in the original version of expmod leverages the idea of successive
; squaring to only require one additional multiplication for each time the exponent doubles.
; Using explicit multiplication nullifies that advantage.