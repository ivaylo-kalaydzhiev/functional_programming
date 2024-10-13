
; ITERATIVE FIB
; what you need to compute the fib at i+1, is fib at i and fib at i-1
; so that is all you need to remember
; you also need to know the base cases'

(define (fib n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (+ 1 i) (+ fi fi-1) fi)))
  (if (= n 0) 0
      (iter 1 1 0)))

; Group 5

; (1)
(+ (/ (+ 3 5) 2)
   (sqrt (- (expt 4 2) (* 7 (expt 2 2)))))

; (2)
(define (my-not x)
  (if x #f #t))

(define (my-and x y)
  (cond (x (if y #t #f))
        (y (if x #t #f))
        (else #f)))


(my-and #t #t)
(my-and #t #f)
(my-and #f #t)
(my-and #f #f)

; (5)
(define (count-digits n)
  (define (helper n acc)
    (if (< n 10) acc (helper (/ n 10) (+ 1 acc))))

  (helper n 1))

; Natural number opertations with Peano definitions

; (9)
(define (succ n) (+ n 1))

; (10)
(define (pred n) (- n 1))

; (11)
(define (add a b)
  (if (= b 0) a
      (succ (add a (pred b)))))

; (12)
(define (mul a b)
  (if (= a 1) b
      (add b (mul (pred a) b))))

; (13)
(define (fact n)
  (if (= n 0) 1
      (mul n (fact (pred n)))))

; (14)
(define (safe-div n)
  
  

      