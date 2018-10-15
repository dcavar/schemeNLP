":"; exec mzscheme -r $0 "$@"

;;; read command line arguments and print line by line
(let loop ((i 0))
  (unless (>= i (vector-length argv))
    (printf "argument: ~a\n" (vector-ref argv i))
    (loop (+ i 1))))

