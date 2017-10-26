#lang racket

(provide root)
(provide LeftSubTree)
(provide RightSubTree)
(provide DisplayTree)

(define (root tree)
  (car tree))

(define (LeftSubTree tree)
  (cadr tree))

(define (RightSubTree tree)
  (caddr tree))

(define (DisplayTree tree)
  (if (empty? tree)
      '()
      (append  (DisplayTree(LeftSubTree tree))(list (root tree)) (DisplayTree (RightSubTree tree)))))

(define tree '(((() 1 ()) 3 (() 4 ())) 9 ((() 12 ()) 13 (() 16 ()))))

(DisplayTree tree) 