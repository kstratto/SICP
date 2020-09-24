#lang racket
; Binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of a certain length, from which hangs either a weight
; or another binary mobile.

; Represent a binary mobile by constructing it as a list of two branches
(define (make-mobile left right)
  (list left right))

; Branch constructed from a numerical length and a structure, which may be either
; a number (representing a simple weight) or another mobile
(define (make-branch length structure)
  (list length structure))

; Part (a)
; Selectors for the branches of a mobile
(define (left-branch mobile)
  (list-ref mobile 0))
(define (right-branch mobile)
  (list-ref mobile 1))
; Selectors which return the components of a branch
(define (branch-length branch)
  (list-ref branch 0))
(define (branch-structure branch)
  (list-ref branch 1))

; Part (b)
; Procedure that returns the total weight of a mobile
; Total weight of a mobile is the sum of the total weights of its branches.
; The weight of a branch is either the value of its structure, if it is a simple weight,
; or the total weight of that structure, if it is a mobile.
; The weight of an empty mobile is zero.
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

; Part (c)
; Say a mobile is balanced if the torque (length of rod multiplied by weight hanging from it)
; applied by the top-left and top-right branches are equal, and if each of the submobiles
; hanging off its branches is balanced
; Procedure that computes the torque of a branch
(define (torque branch)
  (let ((structure (branch-structure branch))
        (length (branch-length branch)))
    (if (not (pair? structure))
        (* length structure)
        (* length (total-weight structure)))))
; Predicate that tests whether a binary mobile is balanced
(define (balanced? mobile)
  (cond ((null? mobile) true) ; Empty mobile is balanced
        ((not (pair? mobile)) true) ; A simple weight on its own is balanced
        ((= (torque (left-branch mobile)) (torque (right-branch mobile)))
         ; If torque applied by left and right branches is equal
         ; Check whether or not the structures of each branch are balanced
         (and (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile)))))
        (else false)))

; Two test cases from the Scheme Wiki
(define m1 (make-mobile
            (make-branch 4 6)
            (make-branch 5 (make-mobile
                            (make-branch 3 7)
                            (make-branch 9 8)))))
; m1 has total weight 21, and it is not balanced, since the left and right branches apply
; different torques
(define m2 (make-mobile
            (make-branch 4 6)
            (make-branch 2 (make-mobile
                            (make-branch 5 8)
                            (make-branch 10 4)))))
; m2 has total weight 18 and it is balanced

; Part (d) if we change the representation of mobiles so the constructors us cons
; instead of list, we just need to change our selectors so that (list-ref list 0)
; is replaced by (car pair) and (list-ref list 1) is replaced by (cdr pair).