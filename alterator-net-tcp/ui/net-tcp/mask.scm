(document:surround "/std/meta-attributes")
(document:insert "/std/functions")

;general all possible ipv4 netmasks
(define (forth n) (bit-extract n 0 8))
(define (third n) (bit-extract n 8 16))
(define (second n) (bit-extract n 16 24))
(define (first n) (bit-extract n 24 32))
(define (ones n) (- (expt 2 n) 1))
(define (gen-mask n) (- (ones 32) (ones (- 32 n))))

(define (string-mask x m)
  (format #f "/~A    (~A.~A.~A.~A)"
          x
          (first m)
          (second m)
          (third m)
          (forth m)))


(define i-am (global 'document:root))

(document:envelop  with-proxy-attributes
                   ((text ((proxy-get self) (number->string (+ (i-am current) 1)))
                          ((proxy-set self value)
                           (let ((value (if (not-empty-string? value) value "32")))
                             (i-am current   (- (string->number value) 1)))))))

type "combobox"
layout-policy 100 -1
rows (map (lambda (x) (string-mask x (gen-mask x))) (iota 32 1))
current 31
