":"; exec mzscheme -r $0 "$@"

(require (lib "vector-lib.ss" "srfi" "43"))

(define load-file
  (lambda (fname)
    (printf "Loading file: ~a\n" fname)
    (call-with-input-file fname
      (lambda (p)
        (read-string (file-size fname) p)))))


;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "~a\n" (load-file fname)))
                   argv))
