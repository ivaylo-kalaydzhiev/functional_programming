; (1)
(define (comp f g)
  (lambda (x) (f (g x))))

; (2)
(define (const c)
  (lambda (x) c))

((const 1) 13)

; (3)
(define (fmax f g)
  (lambda (x)
    (let ((fx (f x))
          (gx (g x)))
      (if (> fx gx) fx gx))))

((fmax (lambda (x) (+ 2 x)) (lambda (x) (+ 1 x))) 0)

; (4)
(define (repeated n f x)
  (if (= n 0)
      x
      (f (repeated (- n 1) f x))))

(repeated 14 (lambda (x) (+ 2 x)) 0)

; (5)
(define (repeat n f)
  (define (helper acc n f)
    (if (= n 0)
        acc
        (helper (lambda (x) (f (acc x)))
                (- n 1)
                f)))
  (helper (lambda (x) x) n f))

((repeat 3 (lambda (x) (+ 1 x))) 0)

; Accumulation

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))


; (6)
; Sum from a to b, where
; if p? curr -> term = 1
; else          term = 0
; next = + 1
(define (count p? a b)
  (accumulate +
              0
              a
              b
              (lambda (x) (if (p? x) 1 0))
              (lambda (x) (+ 1 x))))

(count even? 0 2)

; (7)
(define (any? p? a b)
  (accumulate (lambda (x y) (or x y))
              #f
              a
              b
              (lambda (x) (p? x))
              (lambda (x) (+ 1 x))))

(any? even? 0 12)

; (8)
(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a
              b
              (lambda (x) (p? x))
              (lambda (x) (+ 1 x))))

(all? even? 3 3)

; (9)
; Write repeat with accum
(define (repeated-acc n f x)
  ((repeat-acc n f) x))


; (10)
; Write repeated with accum
(define (id x) x)
(define (1+ x) (+ 1 x))

(define (repeat-acc n f)
  (accumulate comp
              id
              1
              n
              (const f)
              1+))

((repeat-acc 3 1+) 0)
(repeated-acc 3 1+ 0)