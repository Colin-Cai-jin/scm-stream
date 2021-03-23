;The eight queens puzzle
(define (queen n)
 (define (valid? x)
  (define (_valid? x left-pos distance)
   (cond
    ((= distance (length x)) #t)
    ((= distance (abs (- (list-ref x left-pos) (list-ref x (+ left-pos distance))))) #f)
    ((< (+ left-pos distance) (- (length x) 1)) (_valid? x (+ left-pos 1) distance))
    (else (_valid? x 0 (+ distance 1)))))
  (_valid? x 0 1))
 (filter-stream
  valid?
  (permutation-stream (stream->list (range-stream 1 (+ n 1))))))

(for-each-stream
 (lambda (x) (display x)(newline))
 (queen 8))

