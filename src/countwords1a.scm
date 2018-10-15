":"; exec mzscheme -r $0 "$@"

(require (lib "vector-lib.ss" "srfi" "43"))


;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "Got parameter: ~a\n" fname))
                   argv))
