(define (square x) (* x x))

(define (good-enough? x guess)
  (< (abs (- x (square guess))) 0.000001))

(define (improve-guess x guess)
  (/ (+ (/ x guess) guess) 2))

(define (good-enough-2? x guess)
  (< (abs (/ (- guess (improve-guess x guess)) guess)) 0.00000000001))

(define (start-sqrt x guess)
  (if (good-enough-2? x guess)
      guess
      (start-sqrt x (improve-guess x guess))))

(define (sqrt-2 x)
  (start-sqrt x 1.0))

(sqrt-2 0.00000000123456)

(define (improve-guess-c x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-c? x guess)
  (< (abs (/ (- guess (improve-guess-c x guess)) guess)) 0.00000000001))

(define (start-curt x guess)
  (if (good-enough-c? x guess)
      guess
      (start-curt x (improve-guess-c x guess))))

(define (cube-root x)
  (start-curt x 1.0))

(cube-root 0.0064)