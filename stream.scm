(define-syntax tree-cons
 (syntax-rules ()
  ((tree-cons a b) (cons (lambda () a) (lambda () b)))))
(define-syntax tree-car
 (syntax-rules ()
  ((tree-car x) ((car x)))))
(define-syntax tree-cdr
 (syntax-rules ()
  ((tree-cdr x) ((cdr x)))))
(define-syntax make-empty-tree
 (syntax-rules ()
  ((make-empty-tree) '())))
(define-syntax null-tree?
 (syntax-rules ()
  ((null-tree? x) (null? x))))

(define-syntax stream-cons
 (syntax-rules ()
  ((stream-cons a b) (cons a (lambda () b)))))
(define-syntax stream-car
 (syntax-rules ()
  ((stream-car x) (car x))))
(define-syntax stream-cdr
 (syntax-rules ()
  ((stream-cdr x) ((cdr x)))))
(define-syntax make-empty-stream
 (syntax-rules ()
  ((make-empty-stream) '())))
(define-syntax null-stream?
 (syntax-rules ()
  ((null-stream? x) (null? x))))

(define (filter-stream choose? stream)
  (if (null-stream? stream)
      stream
      (let ((x (stream-car stream)))
        (if (choose? x)
            (stream-cons x (filter-stream choose? (stream-cdr stream)))
            (filter-stream choose? (stream-cdr stream))))))

(define (list->stream lst)
  (if (null? lst)
      (make-empty-stream)
      (stream-cons (car lst) (list->stream (cdr lst)))))

(define (stream->list stream)
  (if (null-stream? stream)
      '()
      (cons (stream-car stream) (stream->list (stream-cdr stream)))))

(define (fold-stream f init stream)
  (if (null-stream? stream)
      init
      (fold-stream f (f (stream-car stream) init) (stream-cdr stream))))

(define (for-each-stream f stream)
  (if (null-stream? stream)
      (void)
      (begin
        (f (stream-car stream))
        (for-each-stream f (stream-cdr stream)))))

(define (map-stream f . streams)
  (define (stream-cars streams)
    (if (null? streams)
        '()
        (cons (stream-car (car streams)) (stream-cars (cdr streams)))))
  (define (stream-cdrs streams)
    (if (null? streams)
        '()
        (cons (stream-cdr (car streams)) (stream-cdrs (cdr streams)))))
  (if (null-stream? (car streams))
      (make-empty-stream)
      (stream-cons
       (apply f (stream-cars streams))
       (apply map-stream f (stream-cdrs streams)))))

(define (append-stream . streams)
  (cond
    ((null? streams) (make-empty-stream))
    ((null-stream? (car streams)) (apply append-stream (cdr streams)))
    (else
     (stream-cons
      (stream-car (car streams))
      (apply append-stream (stream-cdr (car streams)) (cdr streams))))))

(define range-stream
  (lambda s
    (define (range3 start end step)
      (cond
        ((and (>= start end) (> step 0)) (make-empty-stream))
        ((and (<= start end) (< step 0)) (make-empty-stream))
        (else (stream-cons start (range3 (+ start step) end step)))))
    (define (range2 start end)
      (range3 start end 1))
    (define (range1 end)
      (range3 0 end 1))
    (cond
      ((null? (cdr s)) (range1 (car s)))
      ((null? (cddr s)) (range2 (car s) (cadr s)))
      (else (range3 (car s) (cadr s) (caddr s))))))

(define (cart-product-stream . lists)
  (define (next s)
    (cond
      ((null? s) #f)
      ((null? (cddar s))
       (let ((n (next (cdr s))))
         (if n
             (cons (cons (caar s) (caar s))  n)
             #f)))
      (else (cons (cons (caar s) (cddar s)) (cdr s)))))
  (define (make s)
    (stream-cons
       (map cadr s)
       (let ((n (next s)))
        (if n
            (make n)
            (make-empty-stream)))))
  (make (map (lambda (x) (cons x x)) lists)))

;Distinguish list and stream
;Only used in the situation that the object is a list or a stream!
(define (stream? obj)
 (cond
  ((null-stream? obj) #t)
  ((procedure? (cdr obj)) #t)
  (else #f)))

;It doesn't work in order
;(define (flat-stream stream)
; (if (null-stream? stream)
;  (make-empty-stream)
;  (append-stream (list->stream (stream-car stream)) (flat-stream (stream-cdr stream)))))

(define (flat-stream stream)
 (if (null-stream? stream)
  (make-empty-stream)
  (let ((x (stream-car stream)))
   (if (null? x)
    (flat-stream (stream-cdr stream))
    (stream-cons
     (car x)
     (flat-stream (stream-cons (cdr x) (stream-cdr stream))))))))

(define (flat-stream-common stream-list/stream-stream)
 (if (null-stream? stream-list/stream-stream)
  (make-empty-stream)
  (let ((x (stream-car stream-list/stream-stream)))
   (if (stream? x)
    (if (null-stream? x)
     (flat-stream-common (stream-cdr stream-list/stream-stream))
     (stream-cons
      (stream-car x)
      (flat-stream-common (stream-cons (stream-cdr x) (stream-cdr stream-list/stream-stream)))))
    (if (null? x)
     (flat-stream (stream-cdr stream-list/stream-stream))
     (stream-cons
      (car x)
      (flat-stream-common (stream-cons (cdr x) (stream-cdr stream-list/stream-stream)))))))))

(define (permutation-stream . args)
 (define (sort-int s)
  (define (insert x s)
   (cond
    ((null? s) (list x))
    ((> x (car s)) (cons x s))
    (else (cons (car s) (insert x (cdr s))))))
  (if (null? s)
   s
   (insert (car s) (sort-int (cdr s)))))
 (define (rm max s)
  (define (merge-rm r v s)
   (cond
    ((< v 0) r)
    ((or (null? s) (> v (car s)) ) (merge-rm (cons v r) (- v 1) s))
    (else (merge-rm r (- v 1) (cdr s)))))
  (merge-rm '() max (sort-int s)))
 (define (make max cnt)
  (if (zero? cnt)
   (stream-cons '() (make-empty-stream))
   (flat-stream
    (map-stream
     (lambda (s) (map (lambda (n) (cons n s)) (rm max s)))
     (make max (- cnt 1))))))
 (if (null? (cdr args))
  (permutation-stream (car args) (length (car args)))
  (map-stream
   (lambda (s) (map (lambda (x) (list-ref (car args) x)) s))
   (make (- (length (car args)) 1) (cadr args)))))

(define (combination-stream lst n)
 (define (create-first)
  (define (it r remain)
   (if (zero? remain)
    r
    (it (cons (- (car r) 1) r) (- remain 1))))
  (it (list (- (length lst) 1)) (- n 1)))
 (define (next s)
  (define (it leval s)
   (define (complete leval s)
    (if (zero? leval)
     s
     (complete (- leval 1) (cons (- (car s) 1) s))))
   (cond
    ((null? s) #f)
    ((< leval (car s)) (complete leval (cons (- (car s) 1) (cdr s))))
    (else (it (+ leval 1) (cdr s)))))
  (it 0 s))
 (define (make s)
  (stream-cons
   (map (lambda (index) (list-ref lst index)) s)
   (let ((s2 (next s)))
    (if s2
     (make s2)
     (make-empty-stream)))))
 (make (create-first)))

(define (power-set-stream lst)
 (if (null? lst)
  (stream-cons '() (make-empty-stream))
  (flat-stream
   (map-stream
    (lambda (x) (list x (cons (car lst) x)))
    (power-set-stream (cdr lst))))))

(define (make-inf-stream init next)
 (stream-cons init (make-inf-stream (next init) next)))

#|
(for-each-stream
 (lambda (x) (display x)(newline))
 (flat-stream
  (map-stream
   append
   (make-inf-stream '(1 2 3 4) (lambda (x) (list (+ 12 (car x)) (+ 13 (car x)) (+ 14 (car x)) (+ 15 (car x)))))
   (make-inf-stream '(5 6 7 8) (lambda (x) (list (+ 12 (car x)) (+ 13 (car x)) (+ 14 (car x)) (+ 15 (car x)))))
   (make-inf-stream '(9 10 11 12) (lambda (x) (list (+ 12 (car x)) (+ 13 (car x)) (+ 14 (car x)) (+ 15 (car x))))))))

(for-each-stream
 (lambda (x) (display x)(newline))
 (filter-stream even?
  (flat-stream
   (filter-stream (lambda (s) (zero? (apply * (map (lambda (x) (remainder x 100)) s))))
    (map-stream
     append
     (make-inf-stream '(1 2 3 4) (lambda (x) (list (+ 12 (car x)) (+ 13 (car x)) (+ 14 (car x)) (+ 15 (car x)))))
     (filter-stream (lambda (s) (= 5 (remainder (car s) 12)))
      (make-inf-stream '(5 6 7 8) (lambda (x) (list (+ 4 (car x)) (+ 5 (car x)) (+ 6 (car x)) (+ 7 (car x))))))
     (make-inf-stream '(9 10 11 12) (lambda (x) (list (+ 12 (car x)) (+ 13 (car x)) (+ 14 (car x)) (+ 15 (car x))))))))))

(for-each-stream
 (lambda (x) (display x)(newline))
 (range-stream 10 1 -2))

(for-each-stream
 (lambda (x) (display x)(newline))
 (cart-product-stream '(1 2 3) '(a b) '(A B C)))

(for-each-stream
 (lambda (x) (display x)(newline))
 (combination-stream '(a b c d e f g) 5))

(for-each-stream
 (lambda (x) (display x)(newline))
 (permutation-stream '(a b c d e f g h i j k l m n o p q r s t u v w x y z) 20))

(for-each-stream
 (lambda (x) (display x)(newline))
 (power-set-stream '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))



|#
