#lang racket/base
(require binary-class racket/class racket/match)
(provide read-jpeg-file-props read-tiff-file-props
         parse-entry read-ifd)

(define (read-jpeg-file-props in)
  (unless (jpeg-goto-marker in #xe1)
    (error 'read-jpeg-file-props
             "could not find JPEG APP1 marker in ~S"
             in))
  (read-value jpeg in))

(define (read-tiff-file-props in)
  (read-value tiff in))

(define (jpeg-goto-marker in marker-byte)
  (and (regexp-try-match (byte-regexp (bytes #xff marker-byte)) in)
       #t))

(define-binary-class jpeg
  ((size l2)
   (_ (constant #"Exif\0\0"))
   (tiff-header tiff)))

(define-binary-class tiff
  ((header current-position)
   (be? tiff-byte-order)
   (_ (if be? (constant (bytes 0 42)) (constant (bytes 42 0))))
   (first-ifd (if be? ifd-be ifd-le) header tiff-hash)))

(struct type-table (int->id id->int))

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

(define-syntax-rule (ifd-template NAME NAME-DATA U2 U4)
  (define-binary-class NAME
    ((offset (or in-offset U4))
     (data (if (zero? offset) #f (ref (+ header offset) NAME-DATA header type-hash))))
    (init-field header type-hash [in-offset #f])))

(ifd-template ifd-be ifd-data-be u2 u4)
(ifd-template ifd-le ifd-data-le l2 l4)

(define-syntax-rule (ifd-data-template NAME-DATA NAME NAME-ENTRY U2 U4)
  (define-binary-class NAME-DATA
    ((current-part (binary-list U2 NAME-ENTRY header type-hash))
     (next-part NAME header type-hash))     
    (init-field header type-hash)))

(ifd-data-template ifd-data-be ifd-be entry-be u2 u4)
(ifd-data-template ifd-data-le ifd-le entry-le l2 l4)

(define-syntax-rule (entry-template NAME-ENTRY NAME U2 U4 INTEGER)
  (define-binary-class NAME-ENTRY 
    (((tag value-type) (binary-hash type-hash U2))
     (type U2)
     (count U4)
     ((binary-type total-bytes) (calculate-type type tag count U2 U4 INTEGER))
     (offset (if (or (ifd? value-type) (> total-bytes 4)) 
                 U4 
                 (bytestring 4)))
     (value (if (ifd? value-type) 
                NAME
                (value-type (tiff-offset binary-type header offset total-bytes count)))
            header
            (if (ifd? value-type) (ifd-hash value-type) #f)
            offset))
    (init-field header type-hash)))

(entry-template entry-be ifd-be u2 u4 integer-be)
(entry-template entry-le ifd-le l2 l4 integer-le)

(define (calculate-type type tag count U2 U4 INTEGER)
  (define-values (binary-type item-size)
    (case type
      [(1) (values u1 1)]
      [(2) (values (iso-8859-1-terminated-string) 1)]
      [(3) (values U2 2)]
      [(4) (values U4 4)]
      [(5) (values (rational U4) 8)]
      [(7) (values u1 1)]
      [(9) (values (signed INTEGER 4) 4)]
      [(10) (values (rational (signed INTEGER 4)) 8)]
      [else (error 'tiff "Bad type ~S, tag = ~S, count = ~S" type tag count)]))  
  (values binary-type (* count item-size)))

(define (tiff-offset type header offset total-bytes count)
  (if (> total-bytes 4)
      (ref (+ header offset) (maybe-many type total-bytes))
      (binary
       (λ (in)
         (define in* (open-input-bytes offset))
         (read-value (maybe-many type total-bytes) in*))
       (λ (out value) ; already written in offset
         (void)))))

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
     
(define (many->maybe-many l)
  (match l
    [(list) #f]
    [(list x) x]
    [(list-rest "" rest) (many->maybe-many rest)]
    [x (reverse x)]))

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

(define (binary-list u2 type . rest)
  (binary
   (λ (in)
     (define entry-count (read-value u2 in))
     (for/list ([_ (in-range entry-count)])
       (apply read-value type in rest)))
   (λ (out value)
     (write-value u2 out (length value))
     (for ([v (in-list value)])
       (write-value type out v)))))

(define (read-ifd ifd)
  (define data (get-field data ifd))
  (if data
      (append (get-field current-part data) (read-ifd (get-field next-part data)))
      null))

(define (parse-entry entry)
  (define value (get-field value entry))
  (list (get-field tag entry)
        (cond
          [(or (is-a? value ifd-be) (is-a? value ifd-le)) (map parse-entry (read-ifd value))]
          [(and (list? value) (= 7 (get-field type entry))) (list->bytes value)]
          [else value])))

(define (make-type-table template)
  (type-table
   (for/hasheqv ([row (in-list template)])
     (match row
       [(list int id binary-type) (values int (list id binary-type))]
       [(list int id) (values int id)]))
   (for/hasheqv ([row (in-list template)])
     (match row
       [(list int id binary-type) (values id (list int binary-type))]
       [(list int id) (values id int)]))))

(define (binary-hash type-table base)
  (binary
   (λ (in)
     (define base-value (read-value base in))
     (define ref (hash-ref (type-table-int->id type-table) base-value base-value))
     (if (list? ref)
         (apply values ref)
         (values ref (λ (base) base))))
   (λ (out value-of-tag type-of-value)
     (define base-value (hash-ref (type-table-id->int type-table) value-of-tag value-of-tag))
     (write-value base out base-value))))

(define-syntax-rule (table-maker NAME FOR)
  (define (NAME template)
    (define table
      (type-table
       (FOR ([row (in-list template)])
         (match row
           [(list val id) (values val id)]))
       (for/hasheqv ([row (in-list template)])
         (match row
           [(list val id) (values id val)]))))
    (λ (base)
      (binary
       (λ (in)
         (define base-value (read-value base in))
         (hash-ref (type-table-int->id table) base-value base-value))
       (λ (out value)
         (define base-value (hash-ref (type-table-id->int table) value value))
         (write-value base out base-value))))))

(table-maker make-type-table* for/hasheqv)
(table-maker make-type-table** for/hash)

(struct ifd (hash))

(define interoperability-hash
  (make-type-table '((1 exif:interoperability:interoperability-index))))

(define canon-makernote-camera-settings
  (make-type-table
   `((1 exif:canon:camera-settings:macro ,(make-type-table
                                           '((1 macro)
                                             (2 normal))))
     (2 exif:canon:camera-settings:self-timer)
     (3 exif:canon:camera-settings:quality ,(make-type-table
                                             '((2 normal)
                                               (3 fine)
                                               (5 superfine))))
     (4 exif:canon:camera-settings:flash-mode ,(make-type-table
                                                '((0 flash-not-fired)
                                                  (1 auto)
                                                  (2 on)
                                                  (3 red-eye-reduction)
                                                  (4 slow-sync)
                                                  (5 auto-and-red-eye-reduction)
                                                  (6 on-and-red-eye-reduction)
                                                  (16 external-flash))))
     (5 exif:canon:camera-settings:drive-mode ,(make-type-table
                                                '((0 single-or-timer)
                                                  (1 continuous))))
     (7 exif:canon:camera-settings:focus-mode ,(make-type-table
                                                '((0 one-shot)
                                                  (1 ai-servo)
                                                  (2 ai-focus)
                                                  (3 manual-3)
                                                  (4 single)
                                                  (5 continuous)
                                                  (6 manual-6))))
     (10 exif:canon:camera-settings:image-size ,(make-type-table
                                                 '((0 large)
                                                   (1 medium)
                                                   (2 small))))
     (11 exif:canon:camera-settings:easy-mode ,(make-type-table
                                                '((0 full-auto)
                                                  (1 manual)
                                                  (2 landscape)
                                                  (3 fast-shutter)
                                                  (4 slow-shutter)
                                                  (5 night)
                                                  (6 black-and-white)
                                                  (7 sepia)
                                                  (8 portrait)
                                                  (9 sports)
                                                  (10 macro-close-up)
                                                  (11 pan-focus))))
     (12 exif:canon:camera-settings:digital-zoom ,(make-type-table
                                                   '((0 none)
                                                     (1 x2)
                                                     (2 x4))))
     (13 exif:canon:camera-settings:contrast ,(make-type-table
                                               '((0 normal)
                                                 (1 high)
                                                 (#xffff low))))
     (14 exif:canon:camera-settings:saturation ,(make-type-table
                                                 '((0 normal)
                                                   (1 high)
                                                   (#xffff low))))
     (15 exif:canon:camera-settings:sharpness ,(make-type-table
                                                '((0 normal)
                                                  (1 high)
                                                  (#xffff low))))
     (16 exif:canon:camera-settings:iso-speed ,(make-type-table
                                                '((15 auto)
                                                  (16 iso-50)
                                                  (17 iso-100)
                                                  (18 iso-200)
                                                  (19 iso-400))))
     (17 exif:canon:camera-settings:metering-mode ,(make-type-table
                                                    '((3 evaluative)
                                                      (4 partial)
                                                      (5 center-weighted))))
     (18 exif:canon:camera-settings:focus-type ,(make-type-table
                                                 '((0 manual)
                                                   (1 auto)
                                                   (3 close-up)
                                                   (4 locked))))
     (19 exif:canon:camera-settings:af-point ,(make-type-table
                                               '((#x3000 none)
                                                 (#x3001 auto-selected)
                                                 (#x3002 right)
                                                 (#x3003 center)
                                                 (#x3004 left))))
     (20 exif:canon:camera-settings:exposure-program ,(make-type-table
                                                       '((0 easy-shooting)
                                                         (1 program)
                                                         (2 tv-priority)
                                                         (3 av-priority)
                                                         (4 manual)
                                                         (5 a-dep))))
     (22 exif:canon:camera-settings:lens-type)
     (23 exif:canon:camera-settings:lens)
     (24 exif:canon:camera-settings:short-focal)
     (25 exif:canon:camera-settings:focal-units)
     (26 exif:canon:camera-settings:max-aperture)
     (27 exif:canon:camera-settings:min-aperture)
     (28 exif:canon:camera-settings:flash-activity ,(make-type-table
                                                     '((0 did-not-fire)
                                                       (1 fired))))
     (29 exif:canon:camera-settings:flash-details) ;; TODO: Decode flash-details bits per http://www.burren.cx/david/canon.html
     (32 exif:canon:camera-settings:focus-continuous ,(make-type-table
                                                       '((0 single)
                                                         (1 continuous))))
     (33 exif:canon:camera-settings:ae-setting)
     (34 exif:canon:camera-settings:image-stabilization)
     (35 exif:canon:camera-settings:display-aperture)
     (36 exif:canon:camera-settings:zoom-source-width)
     (37 exif:canon:camera-settings:zoom-target-width)
     (39 exif:canon:camera-settings:spot-metering-mode)
     (40 exif:canon:camera-settings:photo-effect)
     (41 exif:canon:camera-settings:manual-flash-output)
     (42 exif:canon:camera-settings:color-tone)
     (46 exif:canon:camera-settings:sraw-quality))))

(define canon-makernote-shot-info
  (make-type-table
   `((2 exif:canon:shot-info:iso-speed)
     (3 exif:canon:shot-info:measure-dev)
     (4 exif:canon:shot-info:target-aperture)
     (5 exif:canon:shot-info:target-shutter-speed)
     (7 exif:canon:shot-info:white-balance ,(make-type-table
                                             '((0 auto)
                                               (1 sunny)
                                               (2 cloudy)
                                               (3 tungsten)
                                               (4 fluorescent)
                                               (5 flash)
                                               (6 custom))))
     (9 exif:canon:shot-info:sequence)
     (14 exif:canon:shot-info:af-point-used) ; TODO: Decode bits per http://www.burren.cx/david/canon.html
     (15 exif:canon:shot-info:flash-bias ,(make-type-table
                                           '((#xffc0 decoded:-2.0)
                                             (#xffcc decoded:-1.67)
                                             (#xffd0 decoded:-1.50)
                                             (#xffd4 decoded:-1.33)
                                             (#xffe0 decoded:-1.0)
                                             (#xffec decoded:-0.67)
                                             (#xfff0 decoded:-0.50)
                                             (#xfff4 decoded:-0.33)
                                             (#x0000 decoded:0.0)
                                             (#x000c decoded:0.33)
                                             (#x0010 decoded:0.50)
                                             (#x0014 decoded:0.67)
                                             (#x0020 decoded:1.0)
                                             (#x002c decoded:1.33)
                                             (#x0030 decoded:1.50)
                                             (#x0034 decoded:1.67)
                                             (#x0040 decoded:2.0))))
     (19 exif:canon:shot-info:subject-distance)
     (21 exif:canon:shot-info:aperture-value)
     (22 exif:canon:shot-info:shutter-speed-value)
     (23 exif:canon:shot-info:measure-dev-2))))

(define canon-makernote-panorama
  (make-type-table
   `((2 exif:canon:panorama:panorama-frame)
     (5 exif:canon:panorama:panorama-direction))))

(define canon-makernote-picture-info
  (make-hasheqv
   '((2 exif:canon:picture-info:image-width)
     (3 exif:canon:picture-info:image-height)
     (4 exif:canon:picture-info:image-width-as-shot)
     (5 exif:canon:picture-info:image-height-as-shot)
     (22 exif:canon:picture-info:af-points-used)
     (26 exif:canon:picture-info:af-points-used-20d))))

(define ((canon-list type-table) base)
  (binary
   (λ (in)
     (define val (read-value base in))
     (if (and (pair? val) (integer? (car val)))
         (for/list ([v (in-list val)]
                    [i (in-naturals)])
           (define ref (hash-ref (type-table-int->id type-table) i i))
           (if (list? ref)
               (cons (car ref) (hash-ref (type-table-int->id (cadr ref)) v v))
               (cons ref v)))
         val))
   (λ (out value)
     (write-value base 
                  out
                  (if (pair? value)
                      (for/list ([v (in-list value)]
                                 [i (in-naturals)])
                        (define ref (hash-ref (type-table-int->id type-table) i i))
                        (if (list? ref)
                            (hash-ref (type-table-id->int (cadr ref))
                                      (cdr v)
                                      (cdr v))
                            (cdr v)))
                      value)))))

(define canon-maker-hash
  (make-type-table
   `((1 exif:canon:camera-settings ,(canon-list
                                     canon-makernote-camera-settings))
     (2 exif:canon:focal-length)
     (4 exif:canon:shot-info ,(canon-list canon-makernote-shot-info))
     (5 exif:canon:panorama ,(canon-list canon-makernote-panorama))
     (6 exif:canon:image-type) ;; TODO: %mediafile:trim-canon-markernote-ascii AND MAKE GENERIC PROC NOT HIDE STRINGS
     (7 exif:canon:firmware-version) ;; TODO: %mediafile:trim-canon-markernote-ascii
     (8 exif:canon:file-number)
     (9 exif:canon:owner-name) ;; TODO: %mediafile:trim-canon-markernote-ascii
     (12 exif:canon:serial-number) ;; TODO: !!! decode this to string
     (13 exif:canon:camera-info) ; TODO: !!! HOW DO WE DECODE "camera-info"?
     (15 exif:canon:custom-functions) ;; TODO: Decode this.  Specific to camera model?
     (16 exif:canon:model-id)
     (18 exif:canon:picture-info ,(canon-list canon-makernote-picture-info))
     (19 exif:canon:thumbnail-image-valid-area)
     (21 exif:canon:serial-number-format)
     (26 exif:canon:super-macro)
     (38 exif:canon:af-info)
     (131 exif:canon:original-decision-data-offset)
     (164 exif:canon:white-balance-table)
     (149 exif:canon:lens-model)
     (150 exif:canon:internal-serial-number)
     (151 exif:canon:dust-removal-data)
     (153 exif:canon:custom-functions)
     (160 exif:canon:processing-info)
     (170 exif:canon:measured-color)
     (180 exif:canon:color-space)
     (208 exif:canon:vrd-offset)
     (224 exif:canon:sensor-info)
     (16385 exif:canon:color-data))))

(define exif-hash
  (make-type-table
   `((36864 exif:exif-version)
     (40960 exif:flashpix-version)
     (40961 exif:color-space)
     (42240 exif:gamma)
     (40962 exif:pixel-x-dimension)
     (40963 exif:pixel-y-dimension)
     (37121 exif:components-configuration)
     (37122 exif:compressed-bits-per-pixel)
     (37500 exif:maker-note ,(ifd canon-maker-hash))
     (37510 exif:user-comment) ;; TODO: !!! How do we decode this?  "Type=UNDEFINED" see Exif 2.3 page 46.
     (40964 exif:related-sound-file)
     (36867 exif:date-time-original)
     (36868 exif:date-time-digitized)
     (37520 exif:subsec-time)
     (37521 exif:subsec-time-original)
     (37522 exif:subsec-time-digitized)
     (33434 exif:exposure-time)
     (33437 exif:f-number)
     (34850 exif:exposure-program ,(make-type-table*
                                    '((0 not-defined)
                                      (1 manual)
                                      (2 normal-program)
                                      (3 aperture-priority)
                                      (4 shutter-priority)
                                      (5 creative-program)
                                      (6 action-program)
                                      (7 portrait-mode)
                                      (8 landscape-mode))))
     (34852 exif:spectral-sensitivity)
     (34855 exif:photographic-sensitivity)
     (34856 exif:oecf) ; TODO: Decode?
     (34864 exif:sensitivity-type ,(make-type-table*
                                     '((0 unknown)
                                       (1 sos)
                                       (2 rei)
                                       (3 iso-speed)
                                       (4 sos-and-rei)
                                       (5 sos-and-iso-speed)
                                       (6 rei-and-iso-speed)
                                       (7 sos-and-rei-and-iso-speed))))
     (34865 exif:standard-output-sensitivity)
     (34866 exif:recommended-exposure-index)
     (34867 exif:iso-speed)
     (34868 exif:iso-speed-latitude-yyy)
     (34869 exif:iso-speed-latitude-zzz)
     (37377 exif:shutter-speed-value)
     (37378 exif:aperture-value)
     (37379 exif:brightness-value)
     (37380 exif:exposure-bias-value)
     (37381 exif:max-aperture-value)
     (37382 exif:subject-distance)
     (37383 exif:metering-mode ,(make-type-table*
                                 '((0   unknown)
                                   (1   average)
                                   (2   center-weighted-average)
                                   (3   spot)
                                   (4   multi-spot)
                                   (5   pattern)
                                   (6   partial)
                                   (255 other))))
     (37384 exif:light-source ,(make-type-table*
                                '((0   unknown)
                                  (1   daylight)
                                  (2   fluorescent)
                                  (3   tungsten)
                                  (4   flash)
                                  (9   fine-weather)
                                  (10  cloudy-weather)
                                  (11  shade)
                                  (12  daylight-fluorescent)
                                  (13  day-white-fluorescent)
                                  (14  cool-white-fluorescent)
                                  (15  white-fluorescent)
                                  (16  warm-white-fluorescent)
                                  (17  standard-light-a)
                                  (18  standard-light-b)
                                  (19  standard-light-c)
                                  (20  d55)
                                  (21  d65)
                                  (22  d75)
                                  (23  d50)
                                  (24  iso-studio-tungsten)
                                  (255 other))))
     (37385 exif:flash) ; TODO: Decode!
     (37396 exif:subject-area)
     (37386 exif:focal-length)
     (40965 exif:interoperability-ifd ,(ifd interoperability-hash))
     (41483 exif:flash-energy)
     (41484 exif:spacial-frequency-response)
     (41486 exif:focal-plane-x-resolution)
     (41487 exif:focal-plane-y-resolution)
     (41488 exif:focal-plane-resolution-unit)
     (41492 exif:subject-location)
     (41493 exif:exposure-index)
     (41495 exif:sensing-method ,(make-type-table*
                                  '((1 not-defined)
                                    (2 one-chip-color-area-sensor)
                                    (3 two-chip-color-area-sensor)
                                    (4 three-chip-color-area-sensor)
                                    (5 color-sequential-area-sensor)
                                    (7 trilinear-sensor)
                                    (8 color-sequential-linear-sensor))))
     (41728 exif:file-source ,(make-type-table*
                               '((0 others)
                                 (1 scanner-of-transparent-type)
                                 (2 scanner-of-reflex-type)
                                 (3 dsc))))
     (41729 exif:scene-type ,(make-type-table*
                               '((0 directly-photographed))))
     (41730 exif:cfa-pattern)
     (41985 exif:custom-rendered ,(make-type-table*
                                   '((0 normal-process)
                                     (1 custom-process))))
     (41986 exif:exposure-mode ,(make-type-table*
                                 '((0 auto-exposure)
                                   (1 manual-exposure)
                                   (2 auto-bracket))))
     (41987 exif:white-balance ,(make-type-table*
                                 '((0 auto-white-balance)
                                   (1 manual-white-balance))))
     (41988 exif:digital-zoom-ratio)
     (41989 exif:focal-length-in-35mm-film)
     (41990 exif:scene-capture-type ,(make-type-table*
                                      '((0 standard)
                                        (1 landscape)
                                        (2 portrait)
                                        (3 night-scene))))
     (41991 exif:gain-control ,(make-type-table*
                                '((0 none)
                                  (1 low-gain-up)
                                  (2 high-gain-up)
                                  (3 low-gain-down)
                                  (4 high-gain-down))))
     (41992 exif:contrast ,(make-type-table*
                            '((0 normal)
                              (1 soft)
                              (2 hard))))
     (41993 exif:saturation ,(make-type-table*
                              '((0 normal)
                                (1 low-saturation)
                                (2 high-saturation))))
     (41994 exif:sharpness ,(make-type-table*
                             '((0 normal)
                               (1 soft)
                               (2 hard))))
     (41995 exif:device-setting-description) ; TODO: Decode?
     (41996 exif:subject-distance-range ,(make-type-table*
                                          '((0 unknown)
                                            (1 macro)
                                            (2 close-view)
                                            (3 distant-view))))
     (42016 exif:image-unique-id)
     (42032 exif:camera-owner-name)
     (42033 exif:body-serial-number)
     (42034 exif:lens-specification)
     (42035 exif:lens-make)
     (42036 exif:lens-model)
     (42037 exif:lens-serial-number))))

(define gps-north/south-latitude-val-decode
  (make-type-table**
   '(("N" north-latitude)
     ("S" south-latitude))))

(define gps-east/west-longitude-val-decode
  (make-type-table**
   '(("E" east-longitude)
     ("W" west-longitude))))

(define gps-true/magnetic-direction-val-decode
  (make-type-table**
   '(("T" true-direction)
     ("M" magnetic-direction))))

(define gps-miles/km/knots
  (make-type-table**
   '(("K" kilometers-per-hour)
     ("M" miles-per-hour)
     ("N" knots))))

(define gps-hash
  (make-type-table 
   `((0 exif:gps-version-id)
     (1 exif:gps-latitude-ref ,gps-north/south-latitude-val-decode)
     (2 exif:gps-latitude)
     (3 exif:gps-longitude-ref ,gps-east/west-longitude-val-decode)
     (4 exif:gps-longitude)
     (5 exif:gps-altitude-ref ,(make-type-table*
                                '((0 sea-level)
                                  (1 sea-level-reference-negative-value))))
     (6 exif:gps-altitude)
     (7 exif:gps-timestamp)
     (8 exif:gps-satellites)
     (9 exif:gps-status ,(make-type-table**
                           '(("A" measurement-in-progress)
                             ("V" measurement-interoperability))))
     (10 exif:gps-measure-mode ,(make-type-table**
                                 '(("2" two-dimensional)
                                   ("3" three-dimensional))))
     (11 exif:gps-dop)
     (12 exif:gps-speed-ref ,gps-miles/km/knots)
     (13 exif:gps-speed)
     (14 exif:gps-track-ref ,gps-true/magnetic-direction-val-decode)
     (15 exif:gps-track)
     (16 exif:gps-img-direction-ref ,gps-true/magnetic-direction-val-decode)
     (17 exif:gps-img-direction)
     (18 exif:gps-map-datum)
     (19 exif:gps-dest-latitude-ref ,gps-north/south-latitude-val-decode)
     (20 exif:gps-dest-latitude)
     (21 exif:gps-dest-longitude-ref ,gps-east/west-longitude-val-decode)
     (22 exif:gps-dest-longitude)
     (23 exif:gps-dest-bearing-ref ,gps-true/magnetic-direction-val-decode)
     (24 exif:gps-dest-bearing)
     (25 exif:gps-dest-distance-ref ,gps-miles/km/knots)
     (26 exif:gps-dest-distance)
     (27 exif:gps-processing-method) ; TODO: Decode
     (28 exif:gps-area-information) ; TODO: Decode
     (29 exif:gps-date-stamp)
     (30 exif:gps-differential ,(make-type-table*
                                 '((0 without-correction)
                                   (1 with-correction))))
     (31 exif:gps-h-positioning-error))))

(define tiff-hash
  (make-type-table
   `((254 tiff:new-subfile-type) ;; TODO: !!! burst the bitfields into symbols
     (255 tiff:subfile-type ,(make-type-table*
                              '((1 full-resolution-image)
                                (2 reduced-resolution-image)
                                (3 image-page))))
     (256 tiff:image-width)
     (257 tiff:image-length)
     (258 tiff:bits-per-sample)
     (259 tiff:compression ,(make-type-table*
                             '((1 none)
                               (2 ccitt-1d)
                               (3 group-3-fax)
                               (4 group-4-fax)
                               (5 lzw)
                               (6 jpeg)
                               (32773 packbits))))
     (262 tiff:photometric-interpretation ,(make-type-table*
                                            '((0 white-is-zero)
                                              (1 black-is-zero)
                                              (2 rgb)
                                              (3 palette-color)
                                              (4 transparency-mask)
                                              (5 cmyk)
                                              (6 ycbcr)
                                              (8 cielab))))
     (263 tiff:thresholding ,(make-type-table*
                              `((1 none)
                                (2 ordered)
                                (3 randomized))))
     (264 tiff:cell-width)
     (265 tiff:cell-length)
     (266 tiff:fill-order) ; Note: Don't make symbols for this one.
     (269 tiff:document-name)
     (270 tiff:image-description)
     (271 tiff:make)
     (272 tiff:model)
     (273 tiff:strip-offsets)
     (274 tiff:orientation) ;; TODO: Make symbols?
     (277 tiff:samples-per-pixel)
     (278 tiff:rows-per-strip)
     (279 tiff:strip-byte-counts)
     (280 tiff:min-sample-value)
     (281 tiff:max-sample-value)
     (282 tiff:x-resolution)
     (283 tiff:y-resolution)
     (284 tiff:planar-configuration ,(make-type-table*
                                      '((1 chunky)
                                        (2 planar))))
     (285 tiff:page-name)
     (286 tiff:x-position)
     (287 tiff:y-position)
     (288 tiff:free-offsets)
     (289 tiff:free-byte-counts)
     (290 tiff:gray-response-unit) ; TODO: Fractional number val?
     (291 tiff:gray-response-curve)
     (292 tiff:t4-options) ; TODO: Explode bitfields?
     (293 tiff:t6-options) ; TODO: Explode bitfields?
     (296 tiff:resolution-unit)
     (297 tiff:page-number)
     (301 tiff:transfer-function)
     (305 tiff:software)
     (306 tiff:date-time) ; TODO: convert timestamp?
     (315 tiff:artist)
     (316 tiff:host-computer)
     (317 tiff:predictor ,(make-type-table*
                           '((1 none)
                             (2 horizontal-differencing))))
     (318 tiff:white-point)
     (319 tiff:primary-chromaticities)
     (320 tiff:color-map)
     (321 tiff:halftone-hints)
     (322 tiff:tile-width)
     (323 tiff:tile-length)
     (324 tiff:tile-offsets)
     (325 tiff:tile-byte-counts)
     (332 tiff:ink-set ,(make-type-table*
                         '((1 cmyk)
                           (2 not-cmyk))))
     (333 tiff:ink-names)
     (334 tiff:number-of-inks)
     (336 tiff:dot-range)
     (337 tiff:target-printer)
     (338 tiff:extra-samples ,(make-type-table*
                               '((0 unspecified-data)
                                 (1 associated-alpha-data)
                                 (2 unassociated-alpha-data))))
     (339 tiff:sample-format ,(make-type-table*
                               '((1 unsigned-integer)
                                 (2 twos-complement-signed-integer)
                                 (3 ieee-floating-point)
                                 (4 undefined))))
     (340 tiff:s-min-sample-value)
     (341 tiff:s-max-sample-value)
     (342 tiff:transfer-range)
     (512 tiff:jpeg-proc ,(make-type-table*
                           '((1  baseline-sequential)
                             (14 lossless-with-huffman))))
     (513 tiff:jpeg-interchange-format)
     (514 tiff:jpeg-interchange-format-length)
     (515 tiff:jpeg-restart-interval)
     (517 tiff:jpeg-lossless-predictors)
     (518 tiff:jpeg-point-transforms)
     (519 tiff:jpeg-q-tables)
     (520 tiff:jpeg-dc-tables)
     (521 tiff:jpeg-ac-tables)
     (529 tiff:ycbcr-coefficients)
     (530 tiff:ycbcr-sub-sampling)
     (531 tiff:ycbcr-positioning ,(make-type-table*
                                   '((1 centered)
                                     (2 co-sited))))
     (532 tiff:reference-black-white)
     (33432 tiff:copyright)

     (34665 tiff:exif-ifd ,(ifd exif-hash))
     (34853 tiff:gps-ifd ,(ifd gps-hash)))))

(module+ test
  (call-with-input-file "sony-d700.jpg"
    read-jpeg-file-props))

