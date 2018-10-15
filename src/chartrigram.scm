":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: chartrigrams.ss
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
;;; 2. Trigrams of characters are created from the corpus.
;;; 3. The hash-table is converted into a list of key-value tuples.
;;; 4. The key-values are sorted by value, and a list of tokens
;;;    and their relative frequency is printed out.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r chartrigrams.ss test1.txt test2.txt ...
;;; ----------------------------------------------------


;;; all required libraries and functions
(require (lib "vector-lib.ss" "srfi" "43")) ; for vector-for-each
(require (lib "list.ss"))                   ; for sort


;;; Global variables
(define trigramcount 0.0)                      ; counter of total number tokens
(define trigrams     (make-hash-table 'equal)) ; hash-table for tokens and counts


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


;;; add-words
;;; <- list of characters, i.e. string
;;; !-> updated hash-table trigrams
;;; !-> updated trigramcount counter
;;; ----------------------------------------------------
;;; Add words/tokens from an ordered list of tokens to the hash-table
;;; container and keep track of their count.
(define add-trigrams
  (lambda (text)
    (let ([max (- (string-length text) 2)])
      (set! trigramcount (+ trigramcount max))  ; increment the total number of tokens
      (let loop ([i 0])
        (let* ([token (substring text i (+ i 3))]
               [value (hash-table-get trigrams token 0.0)])
          (hash-table-put! trigrams token (+ value 1)))
        (if (< i (- max 1))
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


;;; ----------------------------------------------------
;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "Loading file: ~a\n" fname)
                     (add-trigrams (load-file fname)))
                   argv)
  (printf "Number of tokens: ~a\n" trigramcount)
  (printf "Number of types: ~a\n" (hash-table-count trigrams))
  (printf "Type/Token ratio: ~a\n" (/ (hash-table-count trigrams) trigramcount))
  (let ([result (sort-by-value trigrams)])
    (printf "---------------------------------------------------------\n")
    (printf "Sorted decreasing with relative frequency:\n")
    (printf "token\tabsolute frequency\trelative frequency\n")
    (for-each (lambda (a)
                (write (car a))
                (printf "\t~a\t~a\n" (cadr a) (/ (cadr a) trigramcount)))
              (reverse result))))

