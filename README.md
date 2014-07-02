binary-class-exif
=================

Class for reading/changing Exif in JPEG
To test
```racket
(define test (call-with-input-file "yourfile.jpg" read-jpeg-file-props))
(map parse-entry (read-ifd (get-field first-ifd (get-field tiff-header test))))
```
or
```racket
(define test (call-with-input-file "yourfile.jpg" read-tiff-file-props))
(map parse-entry (read-ifd (get-field first-ifd test)))
```



