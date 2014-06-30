#lang racket/base
(require binary-class racket/class racket/match)

(define (jpeg-goto-marker in marker-byte)
  (and (regexp-try-match (byte-regexp (bytes #xff marker-byte)) in)
       #t))

(define (read-jpeg-file-props in)
  (unless (jpeg-goto-marker in #xe1)
    (error 'read-jpeg-file-props
             "could not find JPEG APP1 marker in ~S"
             in))
  (read-value jpeg in))

(module+ test
  (call-with-input-file "sony-d700.jpg"
    read-jpeg-file-props))

(define tiff-byte-order
  (binary
   (λ (in)
     (define byte-order-marker (read-bytes 2 in))
     (cond [(equal? #"II" byte-order-marker) #f]
           [(equal? #"MM" byte-order-marker) #t]
           [else (error 'tiff-byte-order
                        "invalid byte order marker ~S"
                        byte-order-marker)]))
   (λ (out value)
     (if value (write-bytes #"MM" out) (write-bytes #"II" out))
     (void))))

(define (many->maybe-many l)
  (match l
    [(list) #f]
    [(list x) x]
    [(list-rest "" rest) (many->maybe-many rest)]
    [x (reverse x)]))

(define (maybe-many type total-bytes)
  (binary
   (λ (in)
     (define end-pos (+ (file-position in) total-bytes))     
     (let loop ([acc null])
       (if (< (file-position in) end-pos)
           (loop (cons (read-value type in) acc))
           (many->maybe-many acc))))
   (λ (out value)
     (define end-pos (+ (file-position out) total-bytes))
     (cond
       [(list? value) (for ([v (in-list value)]) (write-value type out v))]
       [(eq? value #f)]
       [else (write-value type out value)])
     (let ([current (file-position out)])
       (cond 
         [(> current end-pos)
          (error "Too many data. Exceeded ~S bytes" (- (file-position out) end-pos))]
         [(< current end-pos)
          (write-bytes (make-bytes (- current end-pos)) out)])))))
     

(define (tiff-offset type header offset total-bytes count)
  (if (> total-bytes 4)
      (ref (+ header offset) (maybe-many type total-bytes))
      (binary
       (λ (in)
         (define in* (open-input-bytes offset))
         (read-value (maybe-many type total-bytes) in*))
       (λ (out value) ; already written in offset
         (void)))))

(define (rational type)
  (binary
   (λ (in)
     (define numerator (read-value type in))
     (define denominator (read-value type in))
     (/ numerator denominator))
   (λ (out value)
     (define exact (rationalize (inexact->exact value) 1/65536))
     (write-value type out (numerator exact))
     (write-value type out (denominator exact)))))

(define (calculate-type type tag count U2 U4)
  (define-values (binary-type item-size)
    (case type
      [(1) (values u1 1)]
      [(2) (values (iso-8859-1-terminated-string) 1)]
      [(3) (values U2 2)]
      [(4) (values U4 4)]
      [(5) (values (rational U4) 8)]
      [(7) (values u1 1)]
      [(9) (values (signed U4 4) 4)]
      [(10) (values (rational (signed U4 4)) 8)]
      [else (error 'tiff "Bad type ~S, tag = ~S, count = ~S" type tag count)]))
  (values binary-type (* count item-size)))

(define-syntax-rule (tiff-template NAME be? U2 U4)
  (define-binary-class NAME 
    ((tag U2)
     (type U2)
     (count U4)
     ((binary-type total-bytes) (calculate-type type tag count U2 U4))
     (offset (if (> total-bytes 4) U4 (bytestring 4)))
     (value (tiff-offset binary-type header offset total-bytes count)))
    (init-field header)))

(tiff-template tiff-entry-be #t u2 u4)
(tiff-template tiff-entry-le #f l2 l4)

(define (tiff-entry be?)
  (if be? tiff-entry-be tiff-entry-le))

;(define (trace x) (displayln x) x)

(define (binary-list be? type . rest)
  (binary
   (λ (in)
     (define entry-count (read-value (if be? u2 l2) in))
     (for/list ([_ (in-range entry-count)])
       (apply read-value type in rest)))
   (λ (out value)
     (write-value (if be? u2 l2) out (length value))
     (for ([v (in-list value)])
       (write-value type out v)))))

(define-syntax-rule (tiff-ifd-template NAME be? U2 U4)
  (define-binary-class NAME
    ((current-part (binary-list be? (tiff-entry be?) header))
     (next-part-offset U4)
     (next-part-data (if (zero? next-part-offset) 
                         #f 
                         (ref (+ header next-part-offset) NAME header)) header))
    (init-field header)))

(tiff-ifd-template tiff-ifd-be #t u2 u4)
(tiff-ifd-template tiff-ifd-le #f l2 l4)

(define-binary-class tiff
  ((header current-position)
   (be? tiff-byte-order)
   (_ (if be? (constant (bytes 0 42)) (constant (bytes 42 0))))
   (first-ifd-offset (if be? u4 l4))
   (first-ifd-data (ref (+ header first-ifd-offset) (if be? tiff-ifd-be tiff-ifd-le) 
                        header))))

(define-binary-class jpeg
  ((size l2)
   (_ (constant #"Exif\0\0"))
   (tiff-header tiff)))
