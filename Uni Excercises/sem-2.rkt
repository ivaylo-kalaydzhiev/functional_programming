; Elective Sem 2 Practice

; (1)

(define (sum-digits n)
  (define (helper acc rest)
    (if (< rest 10) (+ acc rest)
        (helper (+ (remainder rest 10) acc) (quotient rest 10))))
  (helper 0 n))

(sum-digits 234)

; (2)
(define (count-divisors n)
  (define (helper iter acc)
    (if (> iter n) acc
        (if (= 0 (remainder n iter))
            (helper (+ 1 iter) (+ 1 acc))
            (helper (+ 1 iter) acc))))
  (helper 1 0))

(count-divisors 0)

; (3)
(define (prime? n)
  (= 2 (count-divisors n)))

(prime? 2)

; (4)
; TODO: Make tail-recursive
; handle the case n is negative
(define (increasing-digits? n)
  (define (helper a b curr)
    (if (> a b) #f
      (if (= (remainder curr 10) curr) #t
        (and (helper (remainder curr 10) a (quotient curr 10))))))
  (helper (remainder (quotient n 10) 10) (remainder n 10) (quotient n 100)))

(increasing-digits? 579)

; (5)
(define (ends-with-digit? num digit)
  (= (remainder num 10) digit))

(define (ends-with? num end-num)
  (if (= 0 (quotient end-num 10)) (ends-with-digit? num end-num)
      (and (ends-with-digit? num (remainder end-num 10)) (ends-with? (quotient num 10) (quotient end-num 10)))))

(ends-with-digit? 12 2)
(ends-with? 123 24)

; (6)
(define (sum-divisors n)
  (define (helper iter acc)
    (if (> iter n) acc
        (if (= 0 (remainder n iter))
            (helper (+ 1 iter) (+ (+ iter) acc))
            (helper (+ 1 iter) acc))))
  (helper 1 0))
         
(define (perfect? num)
  (= (- (sum-divisors num) num) num))

(perfect? 33550336)
(perfect? 8126)
(perfect? 28)
(perfect? 8128)

; (7)
(define (binary-to-decimal num)
  (define (helper acc exp rest)
    (if (= rest 0) acc
        (helper (+ acc (* (remainder rest 10) (expt 2 exp)))
                (+ 1 exp)
                (quotient rest 10))))
  (helper 0 0 num))

(binary-to-decimal 00111)

; (8)
(define (decimal-to-binary num)
  (define (helper acc exp rest)
    (if (= rest 0) acc
        (helper (+ acc (* (remainder rest 2) (expt 10 exp)))
                (+ 1 exp)
                (quotient rest 2))))
  (helper 0 0 num))

(decimal-to-binary 8)
                
  








        
