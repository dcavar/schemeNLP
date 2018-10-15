":"; exec mzscheme -r $0 "$@"

;;; CommandLine1.ss
;;; (C) 2006 by Damir Cavar
;;; Command line arguments

;;; read command line arguments and print them line by line

(let loop ((i 0))
  (unless (>= i (vector-length argv))
    (printf "argument: ~a\n" (vector-ref argv i))
    (loop (+ i 1))))

