#lang racket

;250201092

; ------------- decimal converting to binary, octal and hexadecimal ---------------------

(define (divide n d)
  (cond ((> d n ) 0)
        (else
         (+ 1 (divide (- n d) d) ))))

(define (decimal-to-binary num)
  (decimal-to-binary-iter num '()))
(define (decimal-to-binary-iter counter binary)
  (cond ((= counter 0) binary)
        (else
         (decimal-to-binary-iter (divide counter 2) (cons (modulo counter 2) binary)))))

(define (decimal-to-octal num)
  (decimal-to-octal-iter num '()))
(define (decimal-to-octal-iter counter octal)
  (cond ((= counter 0) octal)
        (else
         (decimal-to-octal-iter (divide counter 8) (cons (modulo counter 8) octal)))))
  
(define (decimal-to-hexadecimal num)
  (decimal-to-hexadecimal-iter num '()))
(define (decimal-to-hexadecimal-iter counter hexadecimal)
  (cond ((= counter 0) hexadecimal)
        (else
          (decimal-to-hexadecimal-iter (divide counter 16)
              (cons (cond [(= (modulo counter 16) 10) "A"]
                           [(= (modulo counter 16) 11) "B"]
                           [(= (modulo counter 16) 12) "C"]
                           [(= (modulo counter 16) 13) "D"]
                           [(= (modulo counter 16) 14) "E"]
                           [(= (modulo counter 16) 15) "F"]
                           [(< (modulo counter 16) 10) (modulo counter 16)]) hexadecimal))))) 

; --------------------------------------------------------------------------------------------

; ------------ binary converting to decimal, octal and hexadecimal -------------------------

(define (appending-nums num liste)
  (cond ((= num 0) liste)
        (else
         (appending-nums (divide num 10) (cons (modulo num 10) liste)))))
  
(define (binary-to-decimal num)
  (binary-to-decimal-operation (appending-nums num '())))
(define (binary-to-decimal-operation liste)
  (cond ((empty? liste) 0)
        (else
         (+ (* (car liste) (expt 2 (- (length liste) 1))) (binary-to-decimal-operation(cdr liste))))))

(define (binary-to-octal num)
  (decimal-to-octal (binary-to-decimal num)))

(define (binary-to-hexadecimal num)
  (decimal-to-hexadecimal (binary-to-decimal num)))

; ---------------------------------------------------------------------------------------------

; ------- octal converting to decimal, binary and hexadecimal -----------------------

(define (octal-to-decimal num)
  (octal-to-decimal-operation (appending-nums num '())))
(define (octal-to-decimal-operation liste)
  (cond ((empty? liste) 0)
        (else
         (+ (* (car liste) (expt 8 (- (length liste) 1))) (octal-to-decimal-operation(cdr liste))))))

(define (octal-to-binary num)
  (decimal-to-binary (octal-to-decimal num)))

(define (octal-to-hexadecimal num)
  (decimal-to-hexadecimal (octal-to-decimal num)))

; -----------------------------------------------------------------------------------------------------------

; -------- hexadecimal converting to decimal, binary and octal ---------------------

(define (element-control ec)
  (cond [(char=? ec #\A) 10] 
        [(char=? ec #\B) 11]
        [(char=? ec #\C) 12]
        [(char=? ec #\D) 13]
        [(char=? ec #\E) 14]
        [(char=? ec #\F) 15]
        [(char-numeric? ec) (string->number (string ec))]))

(define (hexadecimal-to-decimal num)
  (hexacalculator (string->list num)))
(define (hexacalculator num)
  (cond ((empty? num) 0)
        (else
         (+ (* (element-control (car num)) (expt 16 (- (length num) 1))) (hexacalculator (cdr num))))))
                                                  

(define (hexadecimal-to-binary num)
  (decimal-to-binary(hexadecimal-to-decimal num)))

(define (hexadecimal-to-octal num)
  (decimal-to-octal(hexadecimal-to-decimal num)))

; ---------------------------------------------------------------------------------------------------------------------------

; -------- main function ---------
  
(define (baseconvert num in_base out_base)  
  (cond [(= in_base 2)
         (cond [(= out_base 10) (binary-to-decimal num)]
               [(= out_base 8) (binary-to-octal num)]
               [(= out_base 16) (binary-to-hexadecimal num)])]
         [(= in_base 8)
          (cond [(= out_base 2) (octal-to-binary num)]
                [(= out_base 10) (octal-to-decimal num)]
                [(= out_base 16) (octal-to-hexadecimal num)])]
         [(= in_base 10)
          (cond [(= out_base 2) (decimal-to-binary num)]
                [(= out_base 8) (decimal-to-octal num)]
                [(= out_base 16) (decimal-to-hexadecimal num)])]
         [(= in_base 16)
          (cond [(= out_base 2) (hexadecimal-to-binary num)]
                [(= out_base 8) (hexadecimal-to-octal num)]
                [(= out_base 10) (hexadecimal-to-decimal num)])]))
               
(baseconvert "AC14" 16 8)
(baseconvert "B2F" 16 10)

 ; baseconvert function takes a num as a NUMBER if the number IS NOT HEXADECIMAL, it takes a num as a STRING if the number IS HEXADECIMAL
 ; baseconvert function takes an INPUT BASE as a NUMBER and it takes an OUTPUT BASE as a NUMBER

