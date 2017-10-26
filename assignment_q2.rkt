#lang racket

(provide ins_beg)
(provide ins_end)
(provide count_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

(define (ins_beg el lst)
  (append(cons el '())lst))

(ins_beg 'a '(b c d))

;-------------------------------

(define (ins_end el lst)
  (append lst (cons el '())))

(ins_end 'a '(b c d))

;-------------------------------

(define (count_top_level lst)
  (if(empty? lst)
  0
  (+ 1 (count_top_level(cdr lst)))))

(count_top_level '('(A B) C D E)) ;should return 4

 ;------------------------------

(define (count_instances item lst)
  (cond
    [(empty? lst) 0]
    [(equal? (car lst) item) (+ 1 (count_instances item (cdr lst)))]
    [else (count_instances item (cdr lst))]))

(count_instances '1 '(1 2 3 4 1 2)) ;Should return 2

;-------------------------------

(define (count_instances_tr item lst)
  (count_instances_tr_helper item lst 0))

(define (count_instances_tr_helper item lst total)
  (cond
  [(empty? lst) total]
  [(equal? item (car lst)) (count_instances_tr_helper item (cdr lst) (+ 1 total))]
  [else (count_instances_tr_helper item (cdr lst) total)]))

(count_instances_tr '1 '( 1 2 3 4 1 2)) ;should return 2

;-------------------------------

(define (count_instances_deep item lst)
  (cond
    [(empty? lst) 0]
    [(equal? item (car lst)) (+ 1 (count_instances_deep item (cdr lst)))]
    [(list? (car lst)) (+ (count_instances_deep item (car lst)) (count_instances_deep item (cdr lst)))]
[else (count_instances_deep item (cdr lst))]))

(count_instances_deep '1 '( 1 2 3 ((1 2 3) 4 (3 1)))) ;Should return 3, goes 3 levels "deep"





