#lang racket

(define (div? a m)
  (zero? (modulo a m)))

(define (inc i)
  (+ 1 i))

(define (dec i)
  (- i 1))

(define (add2 i) (+ i 2))

(define (prime? p)
  (if (findf (lambda (x) (div? p x))
             (range 3 (add2 (sqrt p)) 2))
      false
      true))

(define (primes-under lim)
  (cons 2 (filter prime? (range 3 lim 2))))

(define (sum-primes lim)
  (foldl + 2 (append (list 3 5 7) (filter prime? (range 11 lim 2)))))


