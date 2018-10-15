":"; exec mzscheme -r $0 "$@"

(require (lib "vector-lib.ss" "srfi" "43"))

(define load-file
  (lambda (fname)
    (printf "Loading file: ~a\n" fname)))


;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (load-file fname))
                   argv))
