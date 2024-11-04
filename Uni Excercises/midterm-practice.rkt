; Built-in
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

; Syntax practice
; case, cond, define

; Built-in functions
; remainder, quotient, /
; max, min, gcd, lcm
; floor, ceiling, round

; Recursion practice

; Higher-order functions practice

; Working with lists

; Data Structures


; Week 1
(define (fact n)
  (if (<= n 0)
      1
      (* n (fact (- n 1)))))

; quick and dirty
(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (sum-interval a b)
  (define (helper acc a b)
    (if (> a b)
        acc
        (helper (+ acc a) (+ a 1) b)))
  (helper 0 a b))


(define (count-digits num)
  (define (helper acc num)
    (if (< num 10)
        (+ acc 1)
        (helper (+ acc 1) (quotient num 10))))
  (helper 0 (abs num)))

(define (reverse-digits num)
  (define (helper acc num mul)
    (if (< num 10)
        (+ acc (* mul (remainder num 10)))
        (helper (+ acc (* mul (remainder num 10)))
                (quotient num 10)
                (/ mul 10))))
  (helper 0 (abs num) (expt 10 (- (count-digits num) 1))))

(define (sum-list l)
  (define (helper acc l)
    (if (null? l)
        acc
        (helper (+ acc (car l)) (cdr l))))
  (helper 0 l))

