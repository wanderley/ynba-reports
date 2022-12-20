#lang racket

(module+ test
  (require rackunit))

;; Given a list of transaction, print a report.
;;
;; Example of report:
;;
;;     *Top outflow transactions:*
;;     - G2 = $1500
;;       - C2 = $600
;;       - C1 = $300
;;       - C3 = $600
;;     - G1 = $1000
;;       - C2 = $600
;;       - C1 = $300
;;       - C3 = $80
;;       - Others = $20
;;     - G3 = $1000
;;       - C2 = $600
;;       - C1 = $300
;;       - C3 = $80
;;       - Others = $20
;;     - Others = $1000
;;
;;     *Inflow transactions:*
;;     - G1: C1 = $300
;;     - G2: C1 = $80

(struct transaction [group category outflow inflow] #:transparent)

(module+ test
  (define T1 (list (transaction "G2" "C1" 15.0 0.0)
                   (transaction "G1" "C1" 10.0 0.0)
                   (transaction "G3" "C1"  0.0 7.0)
                   (transaction "G4" "C2"  1.0 0.0)
                   (transaction "G4" "C2"  0.0 1.0)
                   (transaction "G5" "C3"  0.0 5.0)
                   (transaction "G2" "C1" 15.0 0.0))))

(define (read-csv path)
  (define content (port->string (open-input-file path)))
  (for/list ([line (string-split content "\n")])
    (for/list ([field (string-split line ",")])
      (string-replace (string-replace field "\"" "") "\r" ""))))

(define (csv->transactions csv)
  (for/list ([line (rest csv)])
    (transaction (list-ref line 5)
                 (list-ref line 6)
                 (string->number (string-replace (list-ref line 8) "$" ""))
                 (string->number (string-replace (list-ref line 9) "$" "")))))

(module+ test
  (check-equal? (aggregate-outflow T1)
                '(("G2" . 30.0) ("G1" . 10.0) ("G4" . 1.0))))
(define (aggregate-outflow ts)
  (sort
   (filter (λ (a) (> (cdr a) 0))
           (for/list ([g (group-by transaction-group ts)])
             (cons (transaction-group (first g))
                   (for/sum ([t g]) (transaction-outflow t)))))
   >= #:key cdr))

(define (group-by key ts)
  (for/fold ([groups '()]
             #:result (reverse groups))
            ([t (sort ts string<=? #:key key)])
    (cond
      [(empty? groups) (list (list t))]
      [(equal? (key (first (first groups)))
               (key t))
       (cons (cons t (first groups))
             (rest groups))]
      [else
       (cons (list t) groups)])))

(module+ test
  (check-equal? (report T1)
                (string-join '("*Top outflow transactions:*"
                               "- G2 = $30.0"
                               "- G1 = $10.0"
                               "- G4 = $1.0")
                             "\n")))
(define (report ts)
  (string-join
   (cons "*Top outflow transactions:*"
         (for/list ([g (aggregate-outflow ts)])
           (format "- ~a = $~a" (car g) (cdr g))))
   "\n"))

(displayln (report
            (csv->transactions
             (read-csv "example.csv"))))
