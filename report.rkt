#lang racket

;; Given a list of transaction, print a report.
;;
;; Example of report:
;;
;;     *Maiores saídas:*
;;     - G1 = $1000
;;       - C1 = $300
;;       - C2 = $600
;;       - C3 = $80
;;       - Outras = $20
;;     - G2 = $1000
;;       - C1 = $300
;;       - C2 = $600
;;       - C3 = $80
;;       - Outras = $20
;;     - G3 = $1000
;;       - C1 = $300
;;       - C2 = $600
;;       - C3 = $80
;;       - Outras = $20
;;     - Outros = $1000
;;
;;     *Maiores entradas:*
;;     - G1: C1 = $300
;;     - G2: C1 = $80

(struct transaction [group category outflow inflow] #:transparent)

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

(csv->transactions
 (read-csv "example.csv"))
