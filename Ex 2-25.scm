#lang racket
; Want to give the combinations of cars and cdrs that will pick 7 from each of the following
; lists.

; x = (1 3 (5 7) 9)
; (car (cdr (car (cdr (cdr x)))))

; y = ((7))
; (car (car y))

; z = (1 (2 (3 (4 (5 (6 7))))))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))