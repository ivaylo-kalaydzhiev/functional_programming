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
; case, cond, define, let, let*

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

; Week 2
(define (sum-digits n)
  (define (helper acc n)
    (if (= n 0)
        acc
        (helper (+ acc (remainder n 10)) (quotient n 10))))
  (helper 0 (abs n)))

(define (count-divisors num)
  (define (helper acc a b)
    (define term (if (= 0 (remainder b a)) 1 0))
    (if (> a b)
        acc
        (helper (+ acc term) (+ 1 a) b)))
  (helper 0 1 num))

(define (prime? num)
  (= 2 (count-divisors num)))

(define (listify num)
  (define (helper acc num)
    (if (< num 10)
        (cons num acc)
        (helper (cons (remainder num 10) acc)
                (quotient num 10))))
  (helper '() num))

(define (increasing-digits? num)
  (apply < (listify num)))

(define (ends-with? num1 num2)
  (define (helper acc num1 num2)
    (if (< num2 10)
        (and acc (= (remainder num1 10) num2))
        (helper (and acc
                     (= (remainder num1 10) (remainder num2 10)))
                (quotient num1 10)
                (quotient num2 10))))
  (helper #t num1 num2))

(define (automorphic? num)
  (ends-with? (* num num) num))

(define (id x) x)
(define (1+ x) (+ 1 x))
(define (&& p q) (and p q))
(define (|| p q) (or p q))
(define (compose f g)
  (lambda (x) (f (g x))))

(define (acc-fact n)
  (accumulate-i * 1 1 n id 1+))

(define (acc-expt x n)
  (accumulate-i * 1 1 n (lambda (a) x) 1+))


; If you can make the .. into a recursive definition - do that
; If you can make the .. into a sum/ product/ and/ or/ func composition - use accum
; -> accum and accum-i are equivalent, unless you have an op that is
;    strictly left/ right associative
; If you can benefit from list representation use it
; If working with lists use folding
; In any case solving the task mathematically first is genius!!!

(define (apply-f f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))


; Week 3
(define (const c) (lambda (x) c))

(define (fmax f g)
  (lambda (x)
    (if (> (f x) (g x)) f g)))


(define (repeat n f)
  (accumulate compose id 1 n (const f) 1+))

(define (repeated n f x)
  ((repeat n f) x))

(define (all p? a b)
  (accumulate && #t a b (lambda (x) (p? x)) 1+))

(define (any? p? a b)
  (accumulate || #f a b (lambda (x) (p? x)) 1+))

(define (count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) 1+))


; Working with lists
; length, append, reverse, list-tail n (list without first n elements)
; list-ref n, member, memv, memq
; ?? from-to, collect
; Lists problem types
; - looping trough lists
; - constructing lists

(define (listify num)
  (define (helper acc num)
    (if (< num 10)
        (cons num acc)
        (helper (cons (remainder num 10) acc) (quotient num 10))))
  (helper '() num))

(define (length-1 lst)
  (define (helper acc lst)
    (if (null? lst)
        acc
        (helper (1+ acc) (cdr lst))))
  (helper 0 lst))

(define (list-at lst pos)
  (if (= pos 0)
      (car lst)
      (list-at (cdr lst) (- pos 1))))


; Week 4
(define (len lst)
  (if (null? lst)
      0
      (+ 1 (len (cdr lst)))))

(define (minimum lst)
  (apply min lst))

(define (any? p? lst)
  (if (null? lst)
      #f
      (or (p? (car lst)) (any? p? (cdr lst)))))

(define (all? p? lst)
  (if (null? lst)
      #t
      (and (p? (car lst)) (all? p? (cdr lst)))))

(define (member? x lst)
  (if (null? lst)
      #f
      (or (= x (car lst)) (member? x (cdr lst)))))

(define (at n lst)
  (if (null? lst)
      #f
      (if (= n 0)
          (car lst)
          (at (- n 1) (cdr lst)))))

(define (push-back x lst)
  (reverse (cons x (reverse lst))))

(define (insert x n lst)
  (if (null? lst)
      (cons x lst)
      (if (= 0 n)
          (cons x lst)
          (cons (car lst) (insert x (- n 1) (cdr lst))))))

(define (range a b)
  (accumulate cons '() a b id 1+))

; Week 5
(define (sum lst)
  (foldr + 0 lst))

(define (len lst)
  (foldr + 0 (map (const 1) lst)))

(define (any? p? lst)
  (foldr || #f (map p? lst)))

(define (all? p? lst)
  (foldr && #t (map p? lst)))

(define (minimum lst)
  (foldr min (car lst) (cdr lst)))

(define (fold-map f lst)
  (foldr (lambda (term acc) (cons (f term) acc)) '() lst))

(define (fold-filter p? lst)
  (foldr (lambda (term acc) (append (if (p? term) (list term) '()) acc)) '() lst))

; '( ) '(1 2 3 4)
; '(1) '(2 3 4)
; '(2 1) '(3 4)
; snoc '() 1 -> '(1 . ()) = '(1)
; snoc '(1) 2 -> '(2 1)
; snoc also prepends, it is just that it takes switches the
; places of the parameters, nothing more

(define (fold-reverse lst)
  (foldl (lambda (acc term) (cons term acc)) '() lst))

(define (take n lst)
  (if (= n 0)
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (= n 0)
      lst
      (drop (- n 1) (cdr lst))))

(define (take-while p? lst)
  (if (not (p? (car lst)))
      '()
      (cons (car lst) (take-while p? (cdr lst)))))


; FINAL PRACTICE

; (1)
(define (listify num)
  (define (helper acc num)
    (if (< num 10)
        (cons num acc)
        (helper (cons (remainder num 10) acc)
                (quotient num 10))))
  (helper '() num))

(define (palindrome? num)
  (let* ((numList (listify num))
          (numListRev (reverse numList)))
    (equal? numList numListRev)))

; (2)
(define (count-palindromes a b)
  (accumulate + 0 a b (lambda (x) (if (palindrome? x) 1 0)) 1+))

; (3)
(define (sum-primes n k)
  (define (helper acc n k)
    (if (zero? n)
        acc
        (if (prime? (1+ k))
            (helper (+ acc (1+ k)) (- n 1) (1+ k))
            (helper acc n (1+ k)))))
  (helper 0 n k))

; (4)
(define (count-divs-by num k)
  (define (helper acc num k)
    (if (not (zero? (remainder num k)))
        acc
        (helper (1+ acc) (quotient num k) k)))
  (helper 0 num k))


(define (count-divs-by-pair num k)
  (cons k (count-divs-by num k)))


(define (potential-factors num)
  (filter prime? (range 0 num)))

(define (all-prime-factors num)
  (map (lambda (x) (count-divs-by-pair num x))
       (potential-factors num)))

(define (prime-factors num)
  (filter (lambda (pair) (not (zero? (cdr pair))))
          (all-prime-factors num)))



; Hard part

