; Final Midterm Practice

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
(define (divides? num i)
  (zero? (remainder num i))) ; modulo?

(define (sum-divisors num)
  (accumulate + 0 1 num (lambda (i) (if (divides? num i) i 0)) 1+))

(define (done? num)
  (= (- (sum-divisors num) 2 num) num))

(define (distance a b)
  (- (max a b) (min a b)))

(define (dones a b)
  (accumulate append
              '()
              a
              b
              (lambda (i) (if (done? i) (list i) '()))
              1+))

(define (distances-to-dones x a b)
  (map (lambda (done) (distance x done)) (dones a b)))

(define (distance-to-closest-done x a b)
  (apply min (distances-to-dones x a b)))

(define (closer-to-done? x a b)
  (and (< (distance-to-closest-done x a b) (distance x a))
       (< (distance-to-closest-done x a b) (distance x b))))

(define (sum-almost-done a b)
  (accumulate +
              0
              a
              b
              (lambda (x) (if (closer-to-done? x a b) x 0))
              1+))



; (2)
(define (apply-op op a1 a2 memory)
  (if (or (symbol? a1) (symbol? a2))
      memory
      (cons (op a1 a2) (cddr memory))))

(define (apply-binary-procedure ins memory)
  (define op (car ins))
  (define n (cdr ins))
  (define (helper n memory)
    (if (zero? n)
        memory
        (helper (- n 1) (apply-op op (car memory) (cadr memory) memory))))
  (helper n memory))

(define (execute ins memory)
  (cond ((or (number? ins)
             (symbol? ins)) (cons ins memory))
        ((procedure? ins) (map (lambda (el) (if (number? el) (ins el) el)) memory))
        ((and (pair? ins)
              (procedure? (car ins))
              (number? (cdr ins))) (apply-binary-procedure ins memory))
        (else memory)))

(define (run-machine instructions)
  (define (helper memory instructions)
    (if (null? instructions)
        memory
        (helper (execute (car instructions) memory) (cdr instructions))))
  (helper '() instructions))


; (3)

(define (majors? a b)
  (or (and (null? a) (null? b))
      (and (= (length a) (length b))
           (<= (car a) (car b))
           (majors? (cdr a) (cdr b)))))

(define (take n lst)
  (define (helper acc n lst)
    (if (zero? n)
        acc
        (helper (cons (car lst) acc) (- n 1) (cdr lst))))
  (reverse (helper '() n lst)))

(define (make-sublists lst n)
  (define (helper acc lst n)
    (if (or (zero? n) (> n (length lst)))
        acc
        (helper (cons (take n lst) acc) (cdr lst) n)))
  (reverse (helper '() lst n)))

(define (is-sublist? a b)
  (member a (make-sublists b (length a))))

(define || (lambda (x y) (or x y)))

(define (any? lst)
  (foldr || #f lst))

(define (is-majoring? a b)
  (any? (map (lambda (sublist) (majors? a sublist))
                 (make-sublists b (length a)))))

(define (is-major? lst)
  (or (<= (length lst) 1)
      (and (is-majoring? (car lst) (cadr lst))
           (is-major? (cdr lst)))))


; Some more

(define (listify num)
  (define (helper acc num)
    (if (< num 10)
        (cons num acc)
        (helper (cons (remainder num 10) acc)
                (quotient num 10))))
  (helper '() num))


(define (palindrome? num)
  (equal? (listify num)
          (reverse (listify num))))

(define (count-palindromes a b)
  (accumulate + 0 a b (lambda (x) (if (palindrome? x) 1 0)) 1+))

(define (count-dividers x)
  (accumulate + 0 1 x (lambda (i) (if (= (remainder x i) 0) 1 0)) 1+))

(define (prime? x)
  (= 2 (count-dividers x)))

(define (sum-primes n k)
  (define (helper acc n k)
    (if (zero? n)
        acc
        (if (prime? (1+ k))
            (helper (+ acc (1+ k)) (- n 1) (1+ k))
            (helper acc n (1+ k)))))
  (helper 0 n k))

(define (pot-factors num)
  (accumulate append
              '()
              1
              num
              (lambda (x) (if (prime? x) (list x) '()))
              1+))

(define (count-divides x num)
  (define (helper acc num)
    (if (divides? num x)
        (helper (+ 1 acc) (quotient num x))
        acc))
  (helper 0 num))

(define (prime-factors-all num)
  (map (lambda (factor) (cons factor (count-divides factor num)))
       (pot-factors num)))

(define (prime-factors num)
  (filter (lambda (pair) (not (zero? (cdr pair)))) (prime-factors-all num)))

(define (increasing? lst)
  (apply <= lst))

(define (all-pair? p? lst)
  (or (<= (length lst) 1)
      (and (p? (car lst) (cadr lst))
           (all-pair? p? (cdr lst)))))

(define (progression? lst)
  (let ((diff (- (cadr lst) (car lst))))
    (all-pair? (lambda (a1 a2) (= diff (- a2 a1))) lst)))

(define (make-set lst)
  (if (null? lst)
      lst
      (if (member (car lst) (cdr lst))
          (make-set (cdr lst))
          (cons (car lst) (make-set(cdr lst))))))

(define (is-set? lst)
  (= (length lst)
     (length (make-set lst))))

(define (union s1 s2)
  (make-set (append s1 s2)))

(define (intersection s1 s2)
  (define (helper acc s1)
    (if (null? s1)
        acc
        (if (member (car s1) s2)
            (helper (cons (car s1) acc) (cdr s1))
            (helper acc (cdr s1)))))
  (helper '() s1))

(define (s-prod el1 s)
  (map (lambda (el2) (cons el1 el2)) s))

(define (product-deep s1 s2)
  (map (lambda (el) (s-prod el s2)) s1))

(define (product s1 s2)
  (apply append (product-deep s1 s2)))

(define (count-occr el lst)
  (foldr (lambda (x acc) (if (= x el) (+ acc 1) acc)) 0 lst))

(define (count-as-pairs lst)
  (map (lambda (el) (cons el (count-occr el lst))) lst))

(define (max-occr lst)
  (apply max (map (lambda (el) (count-occr el lst)) lst)))

(define (most-common lst)
  (car (car (filter (lambda (pair) (= (cdr pair) (max-occr lst)))
               (count-as-pairs lst)))))


(define (scalar-product xs ys)
  (define (helper acc xs ys)
    (if (null? xs)
        acc
        (helper (cons (* (car xs) (car ys)) acc) (cdr xs) (cdr ys))))
  (if (not (equal? (length xs) (length ys)))
      -1
      (reverse (helper '() xs ys))))