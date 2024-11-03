#lang racket/base

; Traversing lists excercises
; Constructing lists excercises

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))
      
; Better can be written with let
(define (minimum l)
  (if (null? l)
      0
      (if (< (car l) (minimum (cdr l)))
          (car l)
          (minimum (cdr l)))))
      
(define (any? p? l)
  (if (null? l)
      #f
      (or (p? (car l))
          (any? p? (cdr l)))))


(define (member? x l)
  (if (null? l)
      #f
      (or (equal? x (car l))
          (member? x (cdr l)))))
          
          
(define (at i l)
  (if (or (< i 0) (>= i (length l)))
      -1000
      (if (= i 0)
          (car l)
          (at (- i 1) (cdr l)))))
          
(define (push-back x l)
  (append l (list x)))
  
(define (reverse1 l)
  (if (null? l)
      l
      (append (reverse (cdr l))
              (list (car l)))))
              
              
(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ 1 a) b))))
      
(define (snoc x l)
  (append l (list x)))
  
; Prepend single element with cons
(cons '() 2)

; Append single element with snoc
(snoc 2 '())

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
            (map f (cdr l)))))
            
(define (filter p? l)
  (if (null? l)
      l
      (if (p? (car l))
          (cons (car l) (filter p? (cdr l)))
          (filter p? (cdr l)))))
          
(filter odd? '(1 2 3 4 5 6 7 8))
      
      
      
      
      
; foldr vs accumulate
; In accumulate I have to be constructing each
; next element using term and next and I have to know
; when I am supposed to stop adding elements
; In short - I have to construct the list dynamically myself
; 
; In foldr I already have the list and I am just applying
; the desired operation
; 
; Very similar, but quite different
; foldl creates an iterative process, where
; the accumulator is the nv
      
      
              
            
              
              
              
              