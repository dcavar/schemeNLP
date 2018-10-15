":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: countbigrams1.ss
;;; Author:   Damir Cavar <dcavar@unizd.hr>
;;;
;;; (C) 2006 by Damir Cavar
;;;
;;; This code is published under the restrictive GPL!
;;; Please find the text of the GPL here:
;;; http://www.gnu.org/licenses/gpl.txt
;;; 
;;; It is free for use, change, etc. as long as the copyright
;;; note above is included in any modified version of the code.
;;; 
;;; This script assumes that the text is raw and encoded in UTF8.
;;;
;;; Functions:
;;; 1. The text file is loaded into memory.
;;; 2. The text is tokenized, i.e. converted into a list of tokens.
;;; 3. Two adjacent tokens are placed into a hash-table as keys, the value
;;;    is the absolute frequency (i.e. count) of each token in the
;;;    text.
;;; 4. The hash-table is converted into a list of key-value tuples.
;;; 5. The key-values are sorted by value, and a list of bigrams
;;;    and their frequency is printed out.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r countbigrams1.ss test1.txt test2.txt ...
;;; ----------------------------------------------------


;;; all necessary libraries and functions
(require (lib "list.ss"))
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))


;;; Global variables
(define bigramcount 0.0)                      ; total number of bigrams
(define bigrams     (make-hash-table 'equal)) ; hash-table container for bigram-count pairs


;;; print-wordlist
;;; <- hash-table of key-value pairs
;;; side effect: print out of tab-delimited key-value pairs per line
;;; ----------------------------------------------------
;;; Pretty print wordlist as tab-delimited key-value lines.
(define print-bigramlist!
  (lambda (table)
    (hash-table-for-each table
                         (lambda (key value)
                           (printf "~a ~a\t~a\n" (car key) (cadr key) value)))))


;;; sort-by-value
;;; <- hash-table
;;; -> list of key-value tuples (lists)
;;; ----------------------------------------------------
;;; Sort a hash-table of key-value pairs by value, by converting it
;;; into a list of key-value tuples and sorting on the value.
(define sort-by-value
  (lambda (table)
    (let ([keyval (hash-table-map table (lambda (key val) (list key val)))])
      (sort keyval (lambda (a b)
                     (< (cadr a) (cadr b)))))))


;;; add-bigrams
;;; <- list of strings, i.e. token list
;;; !-> updated hash-table bigram-hash
;;; !-> updated count-bigrams counter
;;; ----------------------------------------------------
;;; Add word-bigrams from an ordered list of tokens to the hash-table
;;; container and keep track of their count.
(define add-bigrams
  (lambda (words)
    (let ([count (- (length words) 1)])      ; how many bigrams
      (set! bigramcount (+ bigramcount count)) ; remember the total count
      (let loop ([i 1])
        (let* ([bigram (list (list-ref words (- i 1)) (list-ref words i))]  ; create bigram
               [value  (hash-table-get bigrams bigram 0.0)])             ; get the calue for bigram
          (hash-table-put! bigrams bigram (+ value 1)))
        (if (< i count)
            (loop (+ i 1)))))))


;;; load-file
;;; <- string filename
;;; -> string file content
;;; ----------------------------------------------------
;;; Load text from file into a string variable and return it.
(define load-file
  (lambda (name)
      (call-with-input-file name
        (lambda (p)
          (read-string (file-size name) p)))))


;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "Loading file: ~a\n" fname)
                     (add-bigrams (string-tokenize (load-file fname))))
                   argv)
  (printf "Number of tokens: ~a\n" bigramcount)
  (printf "Number of types: ~a\n" (hash-table-count bigrams))
  (printf "---------------------------------------------------------\n")
  ;(print-bigramlist! bigrams)
  (let ([result (sort-by-value bigrams)])
    (printf "Decreasing frequency profile:\n")
    (for-each (lambda (a)
                (let ([bigram (car a)])
                  (printf "~a ~a\t~a\n" (car bigram) (cadr bigram) (cadr a))))
              (reverse result))))
