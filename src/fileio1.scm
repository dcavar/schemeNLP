":"; exec mzscheme -r $0 "$@"

;;; fileio1.ss
;;; (C) 2006 by Damir Cavar
;;; Read filenames from command line arguments

(define (read-all path)
      (call-with-input-file path
        (lambda (p)
          (read-string (file-size path) p))))

(let loop ((i 0))
  (unless (>= i (vector-length argv))
    (let ((text (read-all (vector-ref argv i))))
      (printf text))
    (loop (+ i 1))))
