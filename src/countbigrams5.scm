":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: countbigrams4.ss
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
;;; mzscheme -r countbigrams4.ss test1.txt test2.txt ...
;;; ----------------------------------------------------


;;; all necessary libraries and functions
(require (lib "list.ss"))
(require (lib "string.ss"))   ; for string-uppercase!
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))  ; for vector-for-each
(require (lib "pregexp.ss"))  ; for Perl compatible regular expressions

(load "english.ss")

;;; Global variables
(define bigramcount 0.0)                      ; total number of bigrams
(define bigrams     (make-hash-table 'equal)) ; hash-table container for bigram-count pairs
(define tokencount  0.0)                      ; total number of tokens
(define types       (make-hash-table 'equal)) ; hash-table container for type-count pairs


;;; sort-by-value
;;; <- hash-table
;;; -> list of key-value tuples (lists)
;;; ----------------------------------------------------
;;; Sort a hash-table of key-value pairs by value, by converting it
;;; into a list of key-value tuples and sorting on the value.
(define sort-by-value
  (lambda (table)
    (let ([keyval (hash-table-map table (lambda (key val)
                                          (list key val)))])
      (sort keyval (lambda (a b)
                     (< (cadr a) (cadr b)))))))


;;; log2 of value
;;; Base transformation:
;;;   log2 is the natural log divided by the natural log of 2 (the base)
(define log2
  (lambda (x)
    (/ (log x) (log 2))))


;;; mutual-information of P(x), P(y), P(xy)
;;; calculate pointwise MI as
;;; P(XY) * log2( P(XY) / (P(X) * P(Y)) )
(define mutual-information
  (lambda (px py pxy)
    (* pxy (log2 (/ pxy (* px py))))))



;;; add-bigrams
;;; <- list of strings, i.e. token list
;;; !-> updated hash-table bigram-hash
;;; !-> updated count-bigrams counter
;;; ----------------------------------------------------
;;; Add word-bigrams from an ordered list of tokens to the hash-table
;;; container and keep track of their count.
(define add-bigrams
  (lambda (words)
    (let ([count (- (length words) 1)])          ; how many bigrams
      (set! bigramcount (+ bigramcount count))     ; remember total count of bigrams
      (set! tokencount  (+ tokencount  (length words))) ; remember total count of tokens
      ; add first token to hash-table
      (let ([val (hash-table-get types (car words) 0.0)])
        (hash-table-put! types (car words) (+ val 1)))
      ; loop over the rest and create tokens and bigrams
      (let loop ([i 1])
        ; add next token to hash-table
        (let ([val (hash-table-get types (list-ref words i) 0.0)])
          (hash-table-put! types (list-ref words i) (+ val 1)))
        ; add next bigram to hash-table
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
                     (let ([text (load-file fname)])
                       (string-lowercase! text)
                       (add-bigrams (string-tokenize (pregexp-replace* "[`'-.,!?;:<>()|\"\\]\\[$%/]+" text " ")))
                       ;(add-bigrams (string-tokenize text))
                       ))
                   argv)
  (printf "bigram\tfrequency\trelative frequency\tmutual information\n")
  (let ([result (sort-by-value bigrams)])
    (for-each (lambda (a)
                (let ([bigram (car a)])
                  ;(unless (or (member (car bigram) stopwords) (member (cadr bigram) stopwords))
                    (printf "~a ~a\t~a\t~a\t~a\n"
                            (car bigram)
                            (cadr bigram)
                            (cadr a)
                            (/ (cadr a) bigramcount)
                            (mutual-information (/ (hash-table-get types (car bigram)) tokencount)
                                                (/ (hash-table-get types (cadr bigram)) tokencount)
                                                (/ (cadr a) bigramcount)))
                    ;)
                  ))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (printf "Number of bigrams\t~a\n" bigramcount)
  (printf "Number of bigram-types\t~a\n" (hash-table-count bigrams))
  (printf "Number of tokens\t~a\n" tokencount)
  (printf "Number of types\t~a\n" (hash-table-count types)))
