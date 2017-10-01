#lang racket
(provide (all-defined-out))

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))
(define za (list #f "hi" 14))

;(define (sum1 xs)
;  (if (null? xs) 0
;      (if (number? (car xs))
;          (+ (car xs) (sum1 (cdr xs)))
;          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))


