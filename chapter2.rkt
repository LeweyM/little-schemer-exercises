#lang racket

;little schemer

;lat?>

(define (lat? l)
  (cond
    ((null? l) #t)
    ((not (list? (car l))) (lat? (cdr l)))
    (else #f)))

(lat? (list 1 2 3 5))