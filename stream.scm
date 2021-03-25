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

;`cons' of stream
(define-syntax stream-cons
 (syntax-rules ()
  ((stream-cons a b) (cons a (lambda () b)))))
;`car' of stream
(define-syntax stream-car
 (syntax-rules ()
  ((stream-car x) (car x))))
;`cdr' of stream
(define-syntax stream-cdr
 (syntax-rules ()
  ((stream-cdr x) ((cdr x)))))
;return an empty stream
(define-syntax make-empty-stream
 (syntax-rules ()
  ((make-empty-stream) '())))
;test whether a stream is null or not
(define-syntax null-stream?
 (syntax-rules ()
  ((null-stream? x) (null? x))))

;Just like `filter', except that the object is not a list, it is a stream
;And, the function return a stream
(define (filter-stream choose? stream)
  (if (null-stream? stream)
      stream
      (let ((x (stream-car stream)))
        (if (choose? x)
            (stream-cons x (filter-stream choose? (stream-cdr stream)))
            (filter-stream choose? (stream-cdr stream))))))

;Transform from list to stream
(define (list->stream lst)
  (if (null? lst)
      (make-empty-stream)
      (stream-cons (car lst) (list->stream (cdr lst)))))

;Transform stream list to list
(define (stream->list stream)
  (if (null-stream? stream)
      '()
      (cons (stream-car stream) (stream->list (stream-cdr stream)))))

;Just like `fold-left', except that the object is not a list, it is a stream
(define (fold-stream f init stream)
  (if (null-stream? stream)
      init
      (fold-stream f (f (stream-car stream) init) (stream-cdr stream))))

(define (fold-stream-return f init stat next-stat end? stream)
 (cond
  ((null-stream? stream) init)
  ((end? stat) init)
  (else
   (let ((x (stream-car stream)))
    (fold-stream-return f (f x init) (next-stat x stat) next-stat end? (stream-cdr stream))))))

;Just like `for-each', except that the object is not a list, it is a stream
(define (for-each-stream f stream)
  (if (null-stream? stream)
      (void)
      (begin
        (f (stream-car stream))
        (for-each-stream f (stream-cdr stream)))))

(define (for-each-stream-return f stat next-stat end? stream)
 (cond
  ((null-stream? stream) (void))
  ((end? stat) (void))
  (else
   (begin
    (f (stream-car stream))
    (for-each-stream-return f (next-stat (stream-car stream) stat) next-stat end? (stream-cdr stream))))))

;Just like `map', except that the objects are not lists, they are streams
;And, the function return a stream
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

;Just like `append', except that the objects are not lists, they are streams
;And, the function return a stream
(define (append-stream . streams)
  (cond
    ((null? streams) (make-empty-stream))
    ((null-stream? (car streams)) (apply append-stream (cdr streams)))
    (else
     (stream-cons
      (stream-car (car streams))
      (apply append-stream (stream-cdr (car streams)) (cdr streams))))))

;Return a stream of the Cartesian product of several lists
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

;If a stream is made up of lists, this function splits each list and assembles them into a new stream
(define (flat-stream stream)
 (if (null-stream? stream)
  (make-empty-stream)
  (let ((x (stream-car stream)))
   (if (null? x)
    (flat-stream (stream-cdr stream))
    (stream-cons
     (car x)
     (flat-stream (stream-cons (cdr x) (stream-cdr stream))))))))

;If a stream is made up of lists or stream, this function splits each list/stream and assembles them into a new stream
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

;return a stream of the permutations of a list
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

;return a stream of the combinations of a list
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

;return a stream of the power set of a list
(define (power-set-stream lst)
 (if (null? lst)
  (stream-cons '() (make-empty-stream))
  (flat-stream
   (map-stream
    (lambda (x) (list x (cons (car lst) x)))
    (power-set-stream (cdr lst))))))

;make a stream
(define (make-stream init next end?)
 (if (end? init)
  (make-empty-stream)
  (stream-cons init (make-stream (next init) next end?))))

;make a stream
(define (make-stream-v2 init next stat next-stat end?)
 (if (end? stat)
  (make-empty-stream)
  (stream-cons init (make-stream-v2 (next init stat) next (next-stat init stat) next-stat end?))))

;make an infinite stream
(define (make-inf-stream init next)
 (stream-cons init (make-inf-stream (next init) next)))

;another way to define `make-inf-stream'
(define (make-inf-stream-v2 init next)
 (make-stream init next (lambda (x) #f)))

;Return a stream of the range
(define range-stream
  (lambda s
    (define (range3 start end step)
      (cond
        ((and (>= start end) (> step 0)) (make-empty-stream))
        ((and (<= start end) (< step 0)) (make-empty-stream))
        (else (stream-cons start (range3 (+ start step) end step)))))
    ;another way to define `range3'
    ;(define (range3 start end step)
    ; (let ((end?
    ;	    (if (> step 0)
    ;	     (lambda (x) (>= x end))
    ;	     (lambda (x) (<= x end)))))
    ; (make-stream start (lambda (x) (+ x step)) end?)))
    (define (range2 start end)
      (range3 start end 1))
    (define (range1 end)
      (range3 0 end 1))
    (cond
      ((null? (cdr s)) (range1 (car s)))
      ((null? (cddr s)) (range2 (car s) (cadr s)))
      (else (range3 (car s) (cadr s) (caddr s))))))

;another way to define `cart-product-stream
(define (cart-product-stream-v2 . lists)
 (define (next s)
  (define (_next now max)
   (cond
    ((null? now)#f)
    ((< (car now) (car max)) (cons (cons (+ (car now) 1) (cdr now)) max))
    (else
     (let ((p (_next (cdr now) (cdr max))))
      (if p
       (cons (cons 0 (car p)) max)
       #f)))))
  (_next (car s) (cdr s)))
 (map-stream
  (lambda (s) (map (lambda (x lst) (list-ref lst x)) (car s) lists))
  (make-stream
   (cons (map (lambda (x) 0) lists) (map (lambda (lst) (- (length lst) 1)) lists))
   next
   not)))

;another way to define `combination-stream'
(define (combination-stream-v2 lst n)
 (define (next s)
  (define (_next now min)
   (cond
    ((null? now) #f)
    ((> (car now) min) (cons (- (car now) 1) (cdr now)))
    (else
     (let ((p (_next (cdr now) (+ min 1))))
      (if p
       (cons (- (car p) 1) p)
       #f)))))
 (_next s 0))
 (map-stream
  (lambda (s) (map (lambda (x) (list-ref lst x)) s))
  (make-stream
   (let ((len (length lst)))
    (stream->list (range-stream (- len n) len)))
   next
   not)))

;another way to define `combination-stream'
(define (permutation-stream-v2 . args)
 (define (next s)
  (cond
   ((null? (car s)) #f)
   ((zero? (caar s))
    (let ((n (next (cons (cdar s) (cdr s)))))
     (if n
      (if (member (cdr s) (car n))
       (next (cons (cons (cdr s) (car n)) (cdr s)))
       (cons (cons (cdr s) (car n)) (cdr s)))
      #f)))
   ((member (- (caar s) 1) (cdar s)) (next (cons (cons (- (caar s) 1) (cdar s)) (cdr s))))
   (else (cons (cons (- (caar s) 1) (cdar s)) (cdr s)))))
 (if (null? (cdr args))
  (permutation-stream (car args) (length (car args)))
  (map-stream
   (lambda (s) (map (lambda (x) (list-ref (car args) x)) (car s)))
   (make-stream
    (let ((len (length (car args))))
     (cons (stream->list (range-stream (- len (cadr args)) len)) (- len 1)))
    next
    not))))

;another way to define `power-set-stream'
(define (power-set-stream-v2 lst)
 (define (choose lst bool)
  (cond
   ((null? lst) '())
   ((car bool) (cons (car lst) (choose (cdr lst) (cdr bool))))
   (else (choose (cdr lst) (cdr bool)))))
 (map-stream
  (lambda (bool) (choose lst bool))
  (apply cart-product-stream (make-list (length lst) '(#t #f))))
) 

(for-each-stream
 (lambda (x) (display x)(display " "))
  (power-set-stream-v2 '(a b c d)))



