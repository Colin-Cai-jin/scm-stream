;24 points
(define (play obj nums)
 (define (insert-op op seq)
  (define (it r op seq)
   (cond
    ((null? seq) r)
    ((null? (car seq)) (it (cons (car op) r) (cdr op) (cdr seq)))
    (else (it (cons (car seq) r) op (cdr seq)))))
  (reverse (it '() op seq)))
 (filter-stream
  (lambda (s)
   (let ((res (fold-stream
               (lambda (n r)
                (cond
                 ((not r) #f)
                 ((not (procedure? n)) (cons n r))
                 ((< (length r) 2) #f)
                 ((and (eq? / n) (zero? (car r))) #f)
                 (else (cons (n (cadr r) (car r)) (cddr r)))))
               '()
               (list->stream s))))
    (if res
     (= obj (car res))
     #f)))
  (let ((ops (apply cart-product-stream (make-list (- (length nums) 1) `(,+ ,- ,* ,/))))
        (x (permutation-stream (append (make-list (- (length nums) 1) '()) nums))))
   (flat-stream-common
    (map-stream
     (lambda (seq)
      (map-stream
       (lambda (op)
	(insert-op op seq))
       ops))
     x)))))

(display "Input a list of 4 numbers")
(newline)
(define nums (read));input 4 cards. (4 7 12 3), for exaple.

(for-each-stream
 (lambda (s)
  (define op->string
   (lambda (op)
    (cond
     ((eq? op +) "+")
     ((eq? op -) "-")
     ((eq? op *) "*")
     ((eq? op /) "/")
     (else ""))))
  (define combine
   (lambda (op r)
    (define trans
     (lambda (x)
      (if (number? x) (number->string x) (string-append "(" x ")"))))
    (cons (string-append (trans (cadr r)) (op->string op) (trans (car r))) (cddr r))))
  ((lambda (x) (display x)(newline))
   (car
    (fold-stream
     (lambda (n r)
      (cond
       ((not (procedure? n)) (cons n r))
       (else (combine n r))))
     '()
     (list->stream s)))))
 (play 24 nums))

