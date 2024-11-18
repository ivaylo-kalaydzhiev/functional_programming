;; често използвани функции
(define (id x) x)
(define (1+ x) (+ x 1))
(define (compose f g) (lambda (x) (f (g x))))

;; натрупване от по-висок ред
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; стандартни генератори на списъци
(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

;; фунции от по-висок ред за списъци
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

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

(define (search p l)
  (and (not (null? l)) (or (p (car l)) (search p (cdr l)))))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

; (1)

(define (listify num)
  (define (helper acc num)
    (if (< num 10)
        (cons num acc)
        (helper (cons (remainder num 10) acc) (quotient num 10))))
  (helper '() num))

(define (positions-list num)
  (reverse (collect 1 (length (listify num)) 1+)))

(define (numify lst)
  (define (helper acc mul lst)
    (if (= 1 mul)
        (+ acc (car lst))
        (helper (+ acc (* mul (car lst))) (/ mul 10) (cdr lst))))
  (helper 0 (expt 10 (- (length lst) 1)) lst))

(define (sum-with-position-l num)
  (map + (positions-list num) (listify num)))

(define (sum-with-position num)
  (numify (apply append (map listify (sum-with-position-l num)))))

; (2)

(define (applied-row fl nl)
  (apply append (map (lambda (f) (map f nl)) fl)))

(define (reduced-applied-row fl nl)
  (apply min (applied-row fl nl)))

; Without higher-order func
(define (reduced-applied-rows fm l)
  (define (helper acc fm)
    (if (null? fm)
        acc
        (helper (cons (reduced-applied-row (car fm) l) acc)
                (cdr fm))))
  (helper '() fm))

; Showing how to solve it only with higher-order funcs
(define (reduced-applied-rows-f fm l)
  (foldr (lambda (x acc) (cons (reduced-applied-row x l) acc)) '() fm))

(define (maxmin fm l)
  (apply max (reduced-applied-rows-f fm l)))

; for testing
(define (square x)
  (* x x))

; (3)

(define tbt '(1 (3 ()()) (-3 (4 () ()) (-1 () ()))))

(define (make-length-path-pairs bt)
  (map (lambda (path) (cons (length path) path)) (make-paths-with-best-value bt)))

(define (filtered-minimum-odd-nodes-prod-pairs pairs)
  (define max-length (apply max (map (lambda (p) (cdr p)) pairs)))
  (filter (lambda (p) (= max-length (cdr p))) pairs))

(define (minimum-odd-nodes-prod bt)
  (car (filtered-minimum-odd-nodes-prod-pairs (make-length-path-pairs bt))))