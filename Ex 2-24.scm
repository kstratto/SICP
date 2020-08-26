#lang racket
; First evaluate the following expression in the interpreter:
; (list 1 (list 2 (list 3 4))
; (1 (2 (3 4)))

; The box/pointer diagram and tree diagram come from the S

; As a box and pointer diagram
;   +---+---+  +---+---+
;   | * | *-+->| * | / |
;   +-+-+---+  +-+-+---+
;     |          |   
;     V          V      
;   +---+      +---+---+  +---+---+
;   | 1 |      | * | *-+->| * | / |
;   +---+      +-+-+---+  +---+---+
;                |          |
;                V          V
;              +---+      +---+---+  +---+---+
;              | 2 |      | * | *-+->| * | / |
;              +---+      +-+-+---+  +-+-+---+
;                           |          |
;                           V          V
;                         +---+      +---+
;                         | 3 |      | 4 |
;                         +---+      +---+

; As a tree
; (1 (2 (3 4)))
;      ^
;    /   \
;   1     ^ (2 (3 4))
;       /   \
;      2     ^ (3 4)
;          /   \
;         3     4