":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: average-mi.ss
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
;;;    with their Mutual Information score, as well as the average
;;;    left and right Mutual Information score.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r average-mi.ss test1.txt test2.txt ...
;;; ----------------------------------------------------

(require (lib "list.ss"))
(require (lib "string.ss")) ; for string-uppercase!
(require (lib "string.ss"     "srfi" "13"))
(require (lib "char-set.ss"   "srfi" "14"))
(require (lib "vector-lib.ss" "srfi" "43"))
(require (lib "pregexp.ss"))  ; for Perl compatible regular expressions
;(load "english.ss")


;;; global counters
(define tokencount  0.0) ; total number of words
(define bigramcount 0.0) ; total number of bigrams

;;; hash-table containers
(define types   (make-hash-table 'equal)) ; words and their counts
(define tags    (make-hash-table 'equal))
(define bigramstoto (make-hash-table 'equal)) ; bigrams and their counts
(define bigramstato (make-hash-table 'equal)) ; bigrams and their counts
(define bigramstata (make-hash-table 'equal)) ; bigrams and their counts
(define bigramstota (make-hash-table 'equal)) ; bigrams and their counts

;;; extra hash-tables
(define lefttoken  (make-hash-table 'equal)) ; key = left token in bigram, value = list of bigrams with key left
(define righttoken (make-hash-table 'equal)) ; key = right token in bigram, value = list of bigrams with key right
(define lefttag    (make-hash-table 'equal)) ; key = left token in bigram, value = list of bigrams with key left
(define righttag   (make-hash-table 'equal)) ; key = right token in bigram, value = list of bigrams with key right
(define tags       (make-hash-table 'equal))


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
    ;(printf "Adding tokens and tags...\n")
    ; remember the total counts of tokens and bigrams
    (let ([count (- (length tokenlist) 1)])
      (set! tokencount  (+ tokencount  (length tokenlist)))
      (set! bigramcount (+ bigramcount count))

      ; count the first token in the list
      (hash-table-put! types (caar tokenlist)
                       (+ (hash-table-get types (caar tokenlist) 0.0) 1))
      (hash-table-put! tags (cadar tokenlist)
                       (+ (hash-table-get tags (cadar tokenlist) 0.0) 1))

      ; loop over the rest of the tokens
      (let loop ([i 1])
        (let* ([word     (car  (list-ref tokenlist i))]       ; right token
               [wordp    (car  (list-ref tokenlist (- i 1)))] ; left token
               [tag      (cadr (list-ref tokenlist i))]       ; tag of right token
               [tagp     (cadr (list-ref tokenlist (- i 1)))] ; tag of left token
               [bigramtato (list tagp  word)] ; bigram = previous and current token as list
               [bigramtota (list wordp tag)]  ; bigram = previous and current token as list
               [bigramtoto (list wordp word)] ; bigram = previous and current token as list
               [bigramtata (list tagp  tag)])  ; bigram = previous and current token as list

          ; store the bigram in the value for left and right
          (let ([listval (hash-table-get lefttoken wordp '())])
            (unless (member listval bigramtota)
              (hash-table-put! lefttoken wordp (cons bigramtota listval))))
          (let ([listval (hash-table-get righttoken word '())])
            (unless (member listval bigramtato)
              (hash-table-put! righttoken word (cons bigramtato listval))))
          
          (let ([listval (hash-table-get lefttag tagp '())])
            (unless (member listval bigramtato)
              (hash-table-put! lefttag tagp (cons bigramtato listval))))
          (let ([listval (hash-table-get righttag tag '())])
            (unless (member listval bigramtota)
              (hash-table-put! righttag tag (cons bigramtota listval))))

          ; store tokens and bigrams in their hash-tables
          (hash-table-put! types word
                           (+ (hash-table-get types word 0.0) 1)) ; increment counter for token
          (hash-table-put! tags tag
                           (+ (hash-table-get tags tag 0.0) 1)) ; increment counter for token
          
          (hash-table-put! bigramstoto bigramtoto
                           (+ (hash-table-get bigramstoto bigramtoto 0.0) 1))   ; increment counter for bigram
          (hash-table-put! bigramstato bigramtato
                           (+ (hash-table-get bigramstato bigramtato 0.0) 1))   ; increment counter for bigram
          (hash-table-put! bigramstota bigramtota
                           (+ (hash-table-get bigramstota bigramtota 0.0) 1))   ; increment counter for bigram
          (hash-table-put! bigramstata bigramtata
                           (+ (hash-table-get bigramstata bigramtata 0.0) 1))   ; increment counter for bigram

        ; go back to loop, if more tokens left
        (if (< i count)
            (loop (+ i 1))))))))


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


; MI for token=word and tags in context
(define get-average-lrmi-tag
  (lambda (token)
    (let ([avleft  0.0]
          [avright 0.0]
          [px      (/ (hash-table-get types token 0.0) tokencount)])
      ; for all bigrams with token left, get MI and average it
      (let* ([val    (hash-table-get lefttoken token '())]
             [lenval (length val)])
        (for-each (lambda (bigram)
                    (let ([py  (/ (hash-table-get tags (cadr bigram) 0.0) tokencount)]
                          [pxy (/ (hash-table-get bigramstota bigram 0.0) bigramcount)])
                      (set! avright (+ avright (mutual-information px py pxy)))))
                  val)
        (unless (= 0 lenval)
          (set! avright (/ avright lenval))))
      ; for all bigrams with token right, get MI and average it
      (let* ([val    (hash-table-get righttoken token '())]
             [lenval (length val)])
        (for-each (lambda (bigram)
                    (let ([py  (/ (hash-table-get tags (car bigram) 0.0) tokencount)]
                          [pxy (/ (hash-table-get bigramstato bigram 0.0) bigramcount)])
                      (set! avleft (+ avleft (mutual-information px py pxy)))))
                  val)
        (unless (= 0 lenval)
          (set! avleft (/ avleft (length val)))))
      (list avleft avright))))



; MI for token=tag and words in context
(define get-average-lrmi-token
  (lambda (token)
    (let ([avleft  0.0]
          [avright 0.0]
          [px      (/ (hash-table-get tags token 0.0) tokencount)])
      ; for all bigrams with token left, get MI and average it
      (let* ([val    (hash-table-get lefttag token '())]
             [lenval (length val)])
        (for-each (lambda (bigram)
                    (let ([py  (/ (hash-table-get types (cadr bigram) 0.0) tokencount)]
                          [pxy (/ (hash-table-get bigramstato bigram 0.0) bigramcount)])
                      (set! avright (+ avright (mutual-information px py pxy)))))
                  val)
        (unless (= 0 lenval)
          (set! avright (/ avright lenval))))
      ; for all bigrams with token right, get MI and average it
      (let* ([val    (hash-table-get righttag token '())]
             [lenval (length val)])
        (for-each (lambda (bigram)
                    (let ([py  (/ (hash-table-get types (car bigram) 0.0) tokencount)]
                          [pxy (/ (hash-table-get bigramstota bigram 0.0) bigramcount)])
                      (set! avleft (+ avleft (mutual-information px py pxy)))))
                  val)
        (unless (= 0 lenval)
          (set! avleft (/ avleft (length val)))))
      (list avleft avright))))



; split token into list with (word tag)
(define prepare-brown
  (lambda (tokens)
    (map (lambda (token)
           (list (substring token 0 (string-index token #\/))
                 (substring token (+ (string-index token #\/) 1))))
         tokens)))



(begin
  (vector-for-each (lambda (i fname)
                     (printf "Processing file ~a (~a of ~a)\n" fname (+ i 1) (vector-length argv))
                     (let ([text (load-file fname)])
                       (string-lowercase! text)
                       (add-data (prepare-brown (string-tokenize text)))))
                   argv)
  
  
  (let ([result (sort-by-value types)])
    (printf "token\tfreq\tlMI\tlvMI\trMI\trvMI\n")
    (for-each (lambda (token)
                (let ([MI (get-average-lrmi-tag (car token))])
                  (printf "~a\t~a\t~a\t~a\t~a\t~a\n"
                          (car token)
                          (/ (cadr token) tokencount)
                          (if (< (car MI) (cadr MI))
                              "-"
                              "+")
                          (car MI)
                          (if (< (cadr MI) (car MI))
                              "-"
                              "+")
                          (cadr MI))))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (printf "Number of tokens: ~a\n"  tokencount)
  (printf "Number of types: ~a\n"   (hash-table-count types))
  (printf "Type/Token ratio: ~a\n"  (/ (hash-table-count types) tokencount))
  (printf "Number of bigrams: ~a\n" bigramcount)
  (printf "Number of bigram types: ~a\n" (hash-table-count bigramstoto))
  (printf "Bigram type/token ratio: ~a\n"  (/ (hash-table-count bigramstoto) bigramcount))
  (printf "---------------------------------------------------------\n")
  (let ([result (sort-by-value tags)])
    (printf "Number of tags: ~a\n"   (hash-table-count tags))
    (printf "token\tfreq\tlMI\tlvMI\trMI\trvMI\n")
    (for-each (lambda (token)
                (let ([MI (get-average-lrmi-token (car token))])
                  (printf "~a\t~a\t~a\t~a\t~a\t~a\n"
                          (car token)
                          (/ (cadr token) tokencount)
                          (if (< (car MI) (cadr MI))
                              "-"
                              "+")
                          (car MI)
                          (if (< (cadr MI) (car MI))
                              "-"
                              "+")
                          (cadr MI))))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (let ([result (sort-by-value bigramstoto)])
    (printf "token\tfreq\tMI\n")
    (for-each (lambda (token)
                (let* ([bigram (car token)]
                       [MI (mutual-information (/ (hash-table-get types (car bigram)) tokencount)
                                               (/ (hash-table-get types (cadr bigram)) tokencount)
                                               (/ (cadr token) bigramcount))])
                  (printf "~a ~a\t~a\t~a\n" (car bigram) (cadr bigram) (/ (cadr token) tokencount) MI)))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (let ([result (sort-by-value bigramstota)])
    (printf "token\tfreq\tMI\n")
    (for-each (lambda (token)
                (let* ([bigram (car token)]
                       [MI (mutual-information (/ (hash-table-get types (car bigram)) tokencount)
                                               (/ (hash-table-get tags (cadr bigram)) tokencount)
                                               (/ (cadr token) bigramcount))])
                  (printf "~a ~a\t~a\t~a\n" (car bigram) (cadr bigram) (/ (cadr token) tokencount) MI)))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (let ([result (sort-by-value bigramstato)])
    (printf "token\tfreq\tMI\n")
    (for-each (lambda (token)
                (let* ([bigram (car token)]
                       [MI (mutual-information (/ (hash-table-get tags (car bigram)) tokencount)
                                               (/ (hash-table-get types (cadr bigram)) tokencount)
                                               (/ (cadr token) bigramcount))])
                  (printf "~a ~a\t~a\t~a\n" (car bigram) (cadr bigram) (/ (cadr token) tokencount) MI)))
              (reverse result)))
  (printf "---------------------------------------------------------\n")
  (let ([result (sort-by-value bigramstata)])
    (printf "token\tfreq\tMI\n")
    (for-each (lambda (token)
                (let* ([bigram (car token)]
                       [MI (mutual-information (/ (hash-table-get tags (car bigram)) tokencount)
                                               (/ (hash-table-get tags (cadr bigram)) tokencount)
                                               (/ (cadr token) bigramcount))])
                  (printf "~a ~a\t~a\t~a\n" (car bigram) (cadr bigram) (/ (cadr token) tokencount) MI)))
              (reverse result)))
  )