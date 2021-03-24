(for-each-stream
 (lambda (x) (display x)(display " "))
 (filter-stream even?
  (flat-stream
   (filter-stream
    (lambda (s) (member 0 (map (lambda (x) (remainder x 100)) s)))
    (map-stream
     append
     (filter-stream (lambda (s) (= 1 (remainder (car s) 6)))
      (make-inf-stream '(1 2) (lambda (x) (list (+ 2 (car x)) (+ 3 (car x))))))
     (filter-stream (lambda (s) (= 3 (remainder (car s) 6)))
      (make-inf-stream '(1 2) (lambda (x) (list (+ 2 (car x)) (+ 3 (car x))))))
     (filter-stream (lambda (s) (= 5 (remainder (car s) 6)))
      (make-inf-stream '(1 2) (lambda (x) (list (+ 2 (car x)) (+ 3 (car x)))))))))))
;98 100 102 200 202 204 296 298 300 398 400 402 500 502 504 596 ...
