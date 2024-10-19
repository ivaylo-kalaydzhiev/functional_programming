(define (fixed-point? f x) (= (f x) x))
(define (branch p? f g x) ((if (p? x) f g) x))
(define (id x) x)

; Accumulation

(define (accumulate op nv a b term next)
  (if (> a b) nv
     (op (term a) (accumulate op nv (next a) b term next))))

(define n 3)
(accumulate +
            0
            1
            n
            (lambda (i) (if (= 0 (remainder n i)) i 0))
            (lambda (i) (+ i 1)))


; Higher order functions practice
; CS 5

; 1.1
(define (composition f g)
  (lambda (x) (f (g x))))

; ((composition exp sin) 12)

; 1.2
; f(f(f(x)))
(define (repeated n f x)
  (if (< n 1)
      x
      (repeated (- n 1) f (f x))))

; (repeated 3 exp 0)
; (exp (exp (exp 0)))

; 1.3
(define (repeat n f)
  (define (helper n f acc)
    (if (= n 0)
        acc
        (helper (- n 1)
                f
                (lambda (x) (f (acc x))))))
  (helper n f (lambda (x) x)))
  
(define (1+ x) (+ 1 x))

; (1+ (1+ 0))
; ((repeat 2 1+) 0)



; Accumulation practice
; op - opearation, init - neutral element, f - func, [begin, end]
(define (accumulate-n op init f begin end)
  (if (> begin end)
      init
      (op (f begin) (accumulate-n op init f (+ 1 begin) end))))

(accumulate-n + 0 (lambda (x) x) 0 5)

; 2.1
(define (cool













