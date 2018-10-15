(define word (string #\H #\a #\l #\l #\o #\space #\w #\o #\r #\l #\d))
word
(string-ref word 0)
(string-ref word 1)
(string-ref word 2)
(string-append word "!")
(define something (make-string 3))
(string-length something)
(string-set! something 0 #\?)
(string-set! something 1 #\?)
something
