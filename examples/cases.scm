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
 (list->stream '(0 1 2 3 4))
;0 1 2 3 4
(newline)


