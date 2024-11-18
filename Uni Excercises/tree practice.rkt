(define st '(1 (2 (4 () ())
                  (5 () ()))
               (3 (6 () ())
                  (7 () ()))))

(define (tree? t)
  (or (null? t) 
      (and (list t)
           (= 3 (length t))
           (tree? (cadr t))
           (tree? (caddr t)))))

(define (make-tree root l r) (list root l r))
(define empty-tree '())
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)



(define (tree? t)
  (or (empty-tree? t)
      (and (list? t)
           (= 3 (length t))
           (tree? (left-tree t))
           (tree? (right-tree t)))))

(define (1+ x) (+ 1 x))

(define (depth-tree t)
  (if (empty-tree? t)
      0
      (1+ (max (depth-tree (left-tree t))
               (depth-tree (right-tree t))))))

; Return a subtree with x as its root
(define (memv-tree x t)
  (and (not (empty-tree? t))
       (or (and (equal? x (root-tree t)) t)
           (memv-tree x (left-tree t))
           (memv-tree x (rigth-tree t)))))

(define (leaf-tree? x t)
  (and (not (empty-tree? t))
       (or (and (= x (root-tree t))
                (= 1 (depth-tree t)))
           (leaf-tree? x (left-tree t))
           (leaf-tree? x (right-tree t)))))

(define (pre-order t)
  (if (empty-tree? t)
      '()
      (append (list (root-tree t))
              (pre-order (left-tree t))
              (pre-order (right-tree t)))))

(define (in-order t)
  (if (empty-tree? t)
      '()
      (append (in-order (left-tree t))
              (list (root-tree t))
              (in-order (right-tree t)))))


(define (post-order t)
  (if (empty-tree? t)
      '()
      (append (post-order (left-tree t))
              (post-order (right-tree t))
              (list (root-tree t)))))

(define (map-tree f t)
  (if (empty-tree? t)
      empty-tree
      (make-tree (f (root-tree t))
                 (map-tree f (left-tree t))
                 (map-tree f (right-tree t)))))


(define (height t)
  (if (empty-tree? t)
      0
      (1+ (max (height (left-tree t))
               (height (right-tree t))))))


(define (level n t)
  (if (empty-tree? t)
      '()
      (if (zero? n)
          (list (root-tree t))
          (append (level (- n 1) (left-tree t))
                  (level (- n 1) (right-tree t))))))

(define (leaves t)
  (level (- (height t) 1) t))

(define (count-leaves t)
  (length (leaves t)))


(define (remove-leaves t)
  (if (empty-tree? t)
      t
      (if (= 1 (height t))
          '()
          (make-tree (root-tree t)
                     (remove-leaves (left-tree t))
                     (remove-leaves (right-tree t))))))

(define (invert t)
  (if (empty-tree? t)
      '()
      (make-tree (root-tree t)
                 (right-tree t)
                 (left-tree t))))


(define (bst? t)
  (or (<= (height t) 1)
      (and (>= (root-tree t) (root-tree (left-tree t)))
           (< (root-tree t) (root-tree (right-tree t)))
           (bst? (left-tree t))
           (bst? (right-tree t)))))

(define (balanced? t)
  (or (empty-tree? t)
      (and (<= (abs (- (height (left-tree t))
                       (height (right-tree t))))
               1)
           (balanced? (left-tree t))
           (balanced? (right-tree t)))))

; This is wrong
(define (insert-bst x t)
  (if (empty-tree? t)
      (make-tree x '() '())
      (if (<= x (root-tree t))
          (insert-bst x (left-tree t))
          (insert-bst x (right-tree t)))))