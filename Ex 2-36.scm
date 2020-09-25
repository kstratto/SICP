#lang racket
; accumulate-n is similar to accumulate, except it takes as its third argument a sequence
; of sequences, which are all assumed to have the same number of elements.
; It applies the designated accumulation procedure to combine all the first elements
; of the sequences, all the second elements of the sequences, and so on,
; and returns a sequence of the results.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))
; Value of (accumulate-n + 0 s) should be the sequence (22 26 30), since
; 1 + 4 + 7 + 10 = 22, 2 + 5 + 8 + 11 = 26, and 3 + 6 + 9 + 12 = 30.
(accumulate-n + 0 s)