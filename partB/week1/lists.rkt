#lang racket
(provide (all-defined-out))

;summ all the nums in a list

(define (sum xs)
  (if (null? xs) 0
      (+ (car xs) (sum (cdr xs)))))

(define temp (sum (list 1 2 3 4 5)))

;append
(define (append xs ys)
  (if (null? xs) ys
      (cons (car xs) (append (cdr xs) ys))))

(define test-append (append (list 1 2 3 4 5) (list 3 3 3)))

;map
(define (my-map f xs)
  (if (null? xs) null
      (cons (f (car xs)) (my-map f (cdr xs)))))