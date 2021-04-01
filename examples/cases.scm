(display (stream->list (range-stream 10)))
;(0 1 2 3 4 5 6 7 8 9)
(newline)

(display (stream->list (range-stream 10 20)))
;(10 11 12 13 14 15 16 17 18 19)
(newline)

(display (stream->list (range-stream 10 20 3)))
;(10 13 16 19)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (list->stream '(0 1 2 3 4)))
;0 1 2 3 4
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (filter-stream
  even?
  (range-stream 10)))
;0 2 4 8
(newline)

(display
 (fold-stream
  (lambda (n r) (+ n (* r 10)))
  0
  (range-stream 1 5)))
;1234
(newline)

(display
 (fold-stream-return
  (lambda (n r) (+ n (* r 10)))
  0
  0
  (lambda (n stat) (+ stat 1))
  (lambda (stat) (>= stat 4))
  (range-stream 1 10)))
;1234
(newline)

(for-each-stream-return
 (lambda (x) (display x)(display " "))
  0
  (lambda (n stat) (+ stat 1))
  (lambda (stat) (>= stat 4))
  (range-stream 1 10))
;1 2 3 4
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (map-stream
  (lambda (a b) (+ (* a 10) b))
  (list->stream '(1 2 3 4))
  (list->stream '(5 6 7 8))))
;15 26 37 48
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (append-stream
  (list->stream '(1 2 3 4))
  (list->stream '(5 6 7 8))))
;1 2 3 4 5 6 7 8
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (cart-product-stream '(1 2) '(a b)))
;you can use `cart-product-stream-v2' instead. Maybe the output sequence is not the same
;(1 a) (2 a) (1 b) (2 b)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (flat-stream
  (map-stream
   list
   (list->stream '(1 2 3 4))
   (list->stream '(5 6 7 8)))))
;1 5 2 6 3 7 4 8
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (flat-stream-common
  (map-stream
   (lambda (a b) (if (even? a) (list a b) (list->stream (list a b))))
   (list->stream '(1 2 3 4))
   (list->stream '(5 6 7 8)))))
;1 5 2 6 3 7 4 8
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (permutation-stream '(1 2 3)))
;you can use `permutation-stream-v2' instead. Maybe the output sequence is not the same
;(3 2 1) (2 3 1) (3 1 2) (1 3 2) (2 1 3) (1 2 3)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (permutation-stream '(1 2 3) 2))
;you can use `permutation-stream-v2' instead. Maybe the output sequence is not the same
;(2 1) (3 1) (1 2) (3 2) (1 3) (2 3)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (combination-stream '(1 2 3) 2))
;you can use `combination-stream-v2' instead. Maybe the output sequence is not the same
;(2 3) (1 3) (1 2)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (power-set-stream '(1 2 3)))
;you can use `power-set-stream-v2' instead. Maybe the output sequence is not the same
;() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (make-stream 1 (lambda (x) (* x 2)) (lambda (stat) (> stat 100))))
;1 2 4 8 16 32 64
(newline)

(for-each-stream
 (lambda (x) (display x)(display " "))
 (make-stream-v2
  1
  (lambda (value stat) (* value 2))
  0
  (lambda (value stat) (+ stat 1))
  (lambda (stat) (> stat 10))))
;1 2 4 8 16 32 64 128 256 512 1024
(newline)

;Knapsack
(define weight-max 100)
;weight/price
;Choose the highest total values under `weight-max'
(define items '((5 . 17) (9 . 33) (8 . 15) (17 . 39) (14 . 35) (25 . 51) (12 . 37) (14 . 29) (11 . 28) (21 . 35) (26 . 47) (18 . 37) (19 . 37) (7 . 24) (9 . 19) (12 . 27) (15 . 42) (16 . 45)))
(let ((res
       (fold-stream
	(lambda (n r)
	 (if (<= (apply + (map car n)) weight-max)
	  (let ((total-values (apply + (map cdr n))))
	   (if (> total-values (car r))
	    (cons total-values n)
	    r))
	  r))
	'(0 . ())
	(power-set-stream items))))
 (display "\tweight\tprice")
 (newline)
 (for-each
  (lambda (x)
   (display "\t")
   (display (car x))
   (display "\t")
   (display (cdr x))
   (newline))
  (cdr res))
 (display "total\t")
 (display (apply + (map car (cdr res))))
 (display "\t")
 (display (car res))
 (newline))
;        weight  price
;        5       17
;        9       33
;        8       15
;        17      39
;        12      37
;        11      28
;        7       24
;        15      42
;        16      45
;total   100     280

