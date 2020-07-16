#lang racket
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
  (cc amount 5))

; The order of growth of the space for this procedure is Theta(n), where n is the amount
; to be changed. We can observe this by drawing a tree for the (cc n 1) case and seeing
; that is has a max depth of n when the first denomination of coin is a penny.
; More generally, if the first denomination of coin is worth d1 cents, then the max depth
; will be the ceiling of n/d1, which is still proportional to n.
; For a general number of coin denominations, k, note that we only add k additional steps
; before reaching the (cc n 1) subtree.
; In addition, since the side of the tree starting with (cc n-dk k) contains
; a subtree with a number of nodes proportional to those in the (cc n 1) tree, for
; each value of k, the number of steps used by this process has an order of growth
; of Theta(n^k). To better visualize this, draw trees for (cc n 1), (cc n 2), and (cc n 3).