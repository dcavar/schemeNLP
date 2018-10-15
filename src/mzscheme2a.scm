":"; exec mzscheme -r $0 "$@"

;;; read command line arguments and print line by line
(printf "program: ~a\n" (path->string program))
(let a ([i 0])
  (unless (>= i (vector-length argv))
    (printf "argument: ~a\n" (vector-ref argv i))
    (a (+ i 1))))

