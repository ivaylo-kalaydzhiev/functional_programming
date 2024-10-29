(define (list1? l)
  (or (null? l)
      (and (pair? l) (list1? (cdr l)))))

(define (map2 f l)
  (if (null? l)
      '()
      (cons (f (car l) (map2 (cdr l))))))


(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(filter even? '(1 2 3 4 5 6))

(map (lambda (x) (map (lambda (f) (filter f x))
                      (list negative? zero? positive?)))
     '((-2 1 0) (1 4 -1) (0 0 1)))