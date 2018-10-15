":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: countunibigrams2.ss
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
;;; 2. Punctuation marks are eliminated from the text.
;;; 3. The text is tokenized.
;;; 4. Two adjacent tokens are placed into a hash-table as keys, the value
;;;    is the absolute frequency (i.e. count) of each token in the
;;;    text.
;;; 5. Single tokens are placed in a second hash-table.
;;; 6. Bigrams that do not contain stopwords are printed out, together
;;;    with their Mutual Information score.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r countunibigrams2.ss test1.txt test2.txt ...
;;; ----------------------------------------------------

(require (lib "list.ss"))
(require (lib "string.ss"))   ; for string-uppercase!
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))
(require (lib "pregexp.ss"))  ; for Perl compatible regular expressions
(load "english.ss")


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


;;; load-file (filename)
;;; load text from file into a string variable and return it
(define load-file
  (lambda (name)
      (call-with-input-file name
        (lambda (p)
          (read-string (file-size name) p)))))


(begin 
  (vector-for-each (lambda (i fname)
                     (printf "Loading file: ~a\n" fname)
                     (let ([text (load-file fname)])
                       (string-lowercase! text)
                       (add-data (string-tokenize (pregexp-replace* "[`'-.,!?;:<>()|\"\\]\\[$%/]+" text " ")))))
                   argv)
  (let ((result (sort-by-value bigrams)))
    (printf "bigram\tfreq\trel freq\tMI\n")
    (for-each (lambda (a)
                (unless (or (member (caar a) stopwords)
                            (member (cadar a) stopwords))
                  (printf "~a ~a\t~a\t~a\t~a\n"
                          (caar a)
                          (cadar a)
                          (cadr a)
                          (/ (cadr a) bigramcount)
                          (mutual-information
                           (/ (hash-table-get types (caar a)) tokencount)
                           (/ (hash-table-get types (cadar a)) tokencount)
                           (/ (cadr a) bigramcount)))))
              (reverse result)))
    (printf "---------------------------------------------------------\n")
    (printf "Number of tokens: ~a\n"  tokencount)
    (printf "Number of types: ~a\n"   (hash-table-count types))
    (printf "Type/Token ratio: ~a\n"  (/ (hash-table-count types) tokencount))
    (printf "Number of bigrams: ~a\n" bigramcount)
    (printf "Number of bigram types: ~a\n" (hash-table-count bigrams))
    (printf "Bigram type/token ratio: ~a\n"  (/ (hash-table-count bigrams) bigramcount)))
