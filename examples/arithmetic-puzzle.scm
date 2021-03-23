(define cal (lambda (s) (fold-stream (lambda (n r) (+ n (* r 10))) 0 (list->stream s))))

;abcd*d=dcba
(for-each-stream
 (lambda (x) (display (string-append (number->string (cal x)) "*" (number->string (cadddr x)) "=" (number->string (cal (reverse x)))))(newline))
 (filter-stream
  (lambda (x)
   (and
    (not (= 1 (cadddr x)))
    (not (zero? (car x)))
    (not (zero? (cadddr x)))
    (= (* (list-ref x 3) (cal x)) (cal (reverse x)))))
  (apply cart-product-stream (make-list 4 (stream->list (range-stream 10))))))
;=>1089*9=9801

;abcdef*4=efabcd
(for-each-stream
 (lambda (x) (display (string-append (number->string (cal x)) "*" "4" "=" (number->string (cal (append (cddddr x) (list (car x) (cadr x) (caddr x) (cadddr x)))))))(newline))
 (filter-stream
  (lambda (x)
   (and
    (not (zero? (car x)))
    (= (* (cal x) 4) (cal (append (cddddr x) (list (car x) (cadr x) (caddr x) (cadddr x)))))))
  (apply cart-product-stream (make-list 6 (stream->list (range-stream 10))))))
;=>238095*4=952380
;  190476*4=761904
;  142857*4=571428
