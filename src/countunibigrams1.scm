":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: countunibigrams1.ss
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
;;; 4. Single tokens are placed in a second hash-table.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r countunibigrams1.ss test1.txt test2.txt ...
;;; ----------------------------------------------------


(require (lib "list.ss"))
(require (lib "string.ss")) ; for string-uppercase!
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))


;;; global counters
(define tokencount  0.0) ; total number of words
(define bigramcount 0.0) ; total number of bigrams

;;; hash-table containers
(define types   (make-hash-table 'equal)) ; words and their counts
(define bigrams (make-hash-table 'equal)) ; bigrams and their counts


;;; sort hash table with by value
;;; assuming values = reals/ints
;;; returning a sorted list of key-value tuples (lists)
(define sort-by-value
  (lambda (table)
    (let ([keyval (hash-table-map table (lambda (key val) (list key val)))])
      (sort keyval (lambda (a b)
                     (< (cadr a) (cadr b)))))))


(define add-data
  (lambda (tokenlist)
    (let ([count (- (length tokenlist) 1)])

      ; remember the total count of tokens and bigrams
      (set! tokencount  (+ tokencount  (length tokenlist)))
      (set! bigramcount (+ bigramcount count))

      ; count the first token in the list
      (let ([value (hash-table-get types (car tokenlist) 0.0)])
        (hash-table-put! types (car tokenlist) (+ value 1)))

      ; loop over the rest of the tokens
      (let loop ([i 1])
        (let* ([token  (list-ref       tokenlist i)]              ; token = second element of bigram 
               [bigram (list (list-ref tokenlist (- i 1)) token)] ; bigram = previous and current token as list
               [wvalue (hash-table-get types   token  0.0)]       ; get value for token
               [bvalue (hash-table-get bigrams bigram 0.0)])      ; get value for bigram
          (hash-table-put! types   token  (+ wvalue 1))    ; increment counter for token
          (hash-table-put! bigrams bigram (+ bvalue 1)))   ; increment counter for bigram
        (if (< i count)
            (loop (+ i 1)))))))


;;; load text from file into a string variable and return it
(define load-file
  (lambda (name)
      (call-with-input-file name
        (lambda (p)
          (read-string (file-size name) p)))))


(begin 
  (vector-for-each (lambda (i fname)
                     ; (printf "Loading file: ~a\n" fname)
                     (add-data (string-tokenize (load-file fname))))
                   argv)
  (let ([result (sort-by-value bigrams)])
    (printf "token\tabs. freq.\trel. freq.\n")
    (for-each (lambda (a)
                (printf "~a\t~a\t~a\n" (car a) (cadr a) (/ (cadr a) bigramcount)))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (printf "Number of tokens: ~a\n"  tokencount)
  (printf "Number of types: ~a\n"   (hash-table-count types))
  (printf "Type/Token ratio: ~a\n"  (/ (hash-table-count types) tokencount))
  (printf "Number of bigrams: ~a\n" bigramcount)
  (printf "Number of bigram types: ~a\n" (hash-table-count bigrams))
  (printf "Bigram type/token ratio: ~a\n"  (/ (hash-table-count bigrams) bigramcount)))
