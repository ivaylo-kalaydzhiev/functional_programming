(define (fact n)
  (if (<= n 0) 1 (* n (fact (- n 1)))))

(define (add a b) (+ a b))

(define (count-digits n)
  (define (helper n count)
    (if (< n 10) count (helper (/ n 10) (+ count 1))))
  (helper n 1)
)

; Do later
;(define (sum-digits n) ())

(define (list-sum list)
  (define (helper list sum)
    (if (null? list) sum (helper (cdr list) (+ sum (car list)))))
  (helper list 0)
)

; Remember list, car, cdr, cons

(list-sum '(1 2 3 4))