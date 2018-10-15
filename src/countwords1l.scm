":"; exec mzscheme -r $0 "$@"

(require (lib "vector-lib.ss" "srfi" "43"))
(require (lib "string.ss"     "srfi" "13"))
(require (lib "list.ss"))


; global word storage
(define words (make-hash-table 'equal))
(define tokencount 0.0)


(define load-file
  (lambda (fname)
    (printf "Loading file: ~a\n" fname)
    (call-with-input-file fname
      (lambda (p)
        (read-string (file-size fname) p)))))


(define count-words
  (lambda (wordlist)
    (set! tokencount (+ tokencount (length wordlist)))
    (for-each (lambda (word)
                (let ([value (hash-table-get words word 0.0)])
                  (hash-table-put! words word (+ value 1.0))))
              wordlist)))


(define sort-by-value
  (lambda (table)
    (let ([keyval (hash-table-map table (lambda (key val) (list key val)))])
      (sort keyval (lambda (a b)
                     (< (cadr a) (cadr b)))))))



;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (count-words (string-tokenize (load-file fname))))
                   argv)
  (printf "Number of tokens: ~a\n" tokencount)
  (printf "Number of types: ~a\n" (hash-table-count words))
  (printf "-------------------------------------\n")
  (for-each (lambda (touple)
              (printf "~a\t~a\t~a\n" (car touple) (cadr touple) (/ (cadr touple) tokencount)))
            (reverse (sort-by-value words))))

