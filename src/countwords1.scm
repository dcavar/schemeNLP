":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: countwords1.ss
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
;;; 3. The tokens are placed into a hash-table as keys, the value
;;;    is the absolute frequency (i.e. count) of each token in the
;;;    text.
;;; 4. The hash-table is converted into a list of key-value tuples.
;;; 5. The key-values are sorted by value, and a list of tokens
;;;    and their frequency is printed out, as well as a reversed
;;;    frequency list.
;;; 6. A list of tokens and their relative frequency is printed out.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r countwords1.ss test1.txt test2.txt ...
;;; ----------------------------------------------------

;;; all necessary libraries and functions
(require (lib "list.ss"))
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))


;;; Global variables
(define count-words 0.0)                      ; total number of tokens
(define word-hash   (make-hash-table 'equal)) ; hash-table container for token-count pairs


;;; print-wordlist
;;; <- hash-table of key-value pairs
;;; side effect: print out of tab-delimited key-value pairs per line
;;; ----------------------------------------------------
;;; Pretty print wordlist as tab-delimited key-value lines.
(define print-wordlist!
  (lambda (table)
    (hash-table-for-each table
                         (lambda (key value)
                           (printf "~a\t~a\n" key value)))))


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
;;; <- list of strings, i.e. token list
;;; <- hash-table
;;; !-> updated hash-table word-hash
;;; !-> updated count-words counter
;;; ----------------------------------------------------
;;; Add words/tokens from an ordered list of tokens to the hash-table
;;; container and keep track of their count.
(define add-words
  (lambda (words)
    (set! count-words (+ count-words (length words)))  ; increment the total number of words counter
    (for-each (lambda (token)
                (let ((value (hash-table-get word-hash token 0.0)))
                  (hash-table-put! word-hash token (+ value 1))))
              words)))


;;; load-file
;;; <- string filename
;;; -> string file content
;;; ----------------------------------------------------
;;; Load text from file into a string variable and return it.
(define load-file
  (lambda (name)
    (let ([size (file-size name)])
      (call-with-input-file name
        (lambda (p)
          (read-string size p))))))


;;; ----------------------------------------------------
;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (printf "Loading file: ~a\n" fname)
                     (add-words (string-tokenize (load-file fname))))
                   argv)
  (printf "Number of tokens: ~a\n" count-words)
  (printf "Number of types: ~a\n" (hash-table-count word-hash))
  (printf "Type/Token ratio: ~a\n" (/ (hash-table-count word-hash) count-words))
  (printf "---------------------------------------------------------\n")
  (print-wordlist! word-hash)
  (let ((result (sort-by-value word-hash)))
    (printf "---------------------------------------------------------\n")
    (printf "Sorted increasing:\n")
    (for-each (lambda (a)
                (printf "~a\t~a\n" (car a) (cadr a)))
              result)
    (printf "---------------------------------------------------------\n")
    (printf "Sorted decreasing:\n")
    (for-each (lambda (a)
                (printf "~a\t~a\n" (car a) (cadr a)))
              (reverse result))
    (printf "---------------------------------------------------------\n")
    (printf "Sorted decreasing with relative frequency:\n")
    (printf "token\tabsolute frequency\trelative frequency\n")
    (for-each (lambda (a)
                (printf "~a\t~a\t~a\n" (car a) (cadr a) (/ (cadr a) count-words)))
              (reverse result))))
