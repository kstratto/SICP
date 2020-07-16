#lang racket
; While Alyssa is correct that her alternative procedure would be able to adequately
; compute a^b mod m, it would encounter issues with combinations of a and b that result
; in large values of a^b. This is due to the fact that her procedure first computes
; a^b, and then takes the residue modulo m. When used in our fast prime tester,
; this strategy will cause issues when trying to check for large primes.
; In comparison, the original expmod procedure is written in a way that limits the
; size of the numbers we work with at any given step. This is due to the fact that
; the reduction steps for when b > 1 use the fact that for any integers x, y, and m
; (xy) mod m = (x mod m)*(y mod m) mod m. In other words, the function from the integers
; Z to the integers modulo m Z/mZ is a ring homomorphism and therefore respects
; multiplication. This fact allows us to perform the computation in expmod without
; ever having to deal with numbers much larger than m, as noted in Footnote 46 on page 68.