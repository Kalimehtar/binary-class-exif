binary-class-exif
=================

Class for reading/changing Exif in JPEG

Based on http://planet.racket-lang.org/display.ss?package=mediafile.plt&owner=neil

To test
```racket
(define test (call-with-input-file "yourfile.jpg" read-jpeg-file-props))
(map parse-entry (read-ifd (get-field first-ifd (get-field tiff-header test))))
```
or
```racket
(define test (call-with-input-file "yourfile.tiff" read-tiff-file-props))
(map parse-entry (read-ifd (get-field first-ifd test)))
```



