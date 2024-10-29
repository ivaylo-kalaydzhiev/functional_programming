; Practice 5

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l) (foldr op  init (cdr l)))))

(define (foldl op init l)
  (if (null? l)
      init
      (foldl op (op init (car l)) (cdr l))))


(define (|| x y) (or x y))
(define (&& x y) (and x y))

; (1)
(define (sum l)
  (foldl + 0 l))

; (2)
(define (len l)
  (foldr (lambda (x acc) (+ 1 acc)) 0 l))

(define (const c)
  (lambda (x) c))

(define (map-len l)
  (sum (map (const 1) l)))

; (3)
(define (any? p? l)
  (foldr (lambda (x y) (or (p? x) y)) #f l))

(define (any?-map p? l)
  (foldr || #f (map p? l)))

(define (all?-map p? l)
  (foldr && #t (map p? l)))

; (4)
(define (foldr1 op l)
  (op (car l) (foldr op (car l) (cdr l))))

(define (foldl1 op l)
  (foldl op (op (car l) (car l)) (cdr l)))

; (5)
(define (minimum l) (foldr1 min l))
(define (maximum l) (foldr1 max l))

; (6)
(define (map-f f l)
  (foldr f '() l))

; (7)
(define (filter-f p? l)
  (foldr (lambda (x y) (if (p? x) (cons x y) y)) '() l))

; (8)
(define (reverse-f l)
  (foldl (lambda (x y) (cons y x)) '() l))

(reverse-f '(1 2 3 4))

; (9)
(define (take n l)
  (if (= n 0)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

(define (drop n l)
  (if (= n 0)
      l
      (drop (- n 1) (cdr l))))


; (10)
(define (take-while p? l)
  (foldr (lambda (x y) (if (p? x)
                           (cons x y)
                           '())) '() l))

(define (drop-while p? l)
  (foldr (lambda (x y) (if (p? x)
                           (cdr y)
                           l)) '() l))

; (11)
(define (zip l1 l2)
  (map cons l1 l2))

(define (zip-with op l1 l2)
  (map op l1 l2))
