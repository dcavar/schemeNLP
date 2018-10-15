":"; exec mzscheme -r $0 "$@"

(require (lib "vector-lib.ss" "srfi" "43"))
(require (lib "string.ss"     "srfi" "13"))


(define load-file
  (lambda (fname)
    (printf "Loading file: ~a\n" fname)
    (call-with-input-file fname
      (lambda (p)
        (read-string (file-size fname) p)))))


;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "~a\n" (string-tokenize (load-file fname))))
                   argv))
