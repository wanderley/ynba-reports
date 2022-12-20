#lang racket

(define (read-csv path)
  (define content (port->string (open-input-file path)))
  (for/list ([line (string-split content "\n")])
    (for/list ([field (string-split line ",")])
      (string-replace (string-replace field "\"" "") "\r" ""))))

(read-csv "example.csv")
