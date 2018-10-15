":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: vectorspace2.ss
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

;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r vectorspace2.ss test1.txt test2.txt ...
;;; ----------------------------------------------------

(require (lib "list.ss"))
(require (lib "list.ss"       "srfi" "1" ))
(require (lib "string.ss")) ; for string-uppercase!
(require (lib "string.ss"     "srfi" "13"))
(require (lib "vector-lib.ss" "srfi" "43"))
(require (lib "pregexp.ss"))  ; for Perl compatible regular expressions
(load "k-means.ss")
; (load "english.ss")


;;; global counters
(define tokencount  0) ; total number of words
(define bigramcount 0) ; total number of bigrams

;;; hash-table containers
(define types   (make-hash-table 'equal)) ; words and their counts
(define bigrams (make-hash-table 'equal)) ; bigrams and their counts

;;; extra hash-tables
(define lefttoken  (make-hash-table 'equal)) ; key = left token in bigram, value = list of bigrams with key left
(define righttoken (make-hash-table 'equal)) ; key = right token in bigram, value = list of bigrams with key right


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
      
      ; remember the total counts of tokens and bigrams
      (set! tokencount  (+ tokencount  (length tokenlist)))
      (set! bigramcount (+ bigramcount count))
      
      ; count the first token in the list
      (let ([value (hash-table-get types (car tokenlist) 0.0)])
        (hash-table-put! types (car tokenlist) (+ value 1)))
      
      ; loop over the rest of the tokens
      (let loop ([i 1])
        (let* ([token    (list-ref       tokenlist i)]              ; token = second element of bigram 
               [bigram   (list (list-ref tokenlist (- i 1)) token)] ; bigram = previous and current token as list
               [wvalue   (hash-table-get types   token  0.0)]       ; get value for token
               [bvalue   (hash-table-get bigrams bigram 0.0)]       ; get value for bigram
               [leftval  (hash-table-get lefttoken  (car bigram) '())]  ; list of bigrams where token is left
               [rightval (hash-table-get righttoken (cadr bigram) '())]) ; list of bigrams where token is right
          
          ; store the bigram in the value for left and right
          (unless (member leftval bigram)
            (set! leftval (cons bigram leftval))
            (hash-table-put! lefttoken (car bigram) leftval))
          (unless (member rightval bigram)
            (set! rightval (cons bigram rightval))
            (hash-table-put! righttoken (cadr bigram) rightval))
          
          ; store tokens and bigrams in their hash-tables
          (hash-table-put! types   token  (+ wvalue 1))    ; increment counter for token
          (hash-table-put! bigrams bigram (+ bvalue 1)))   ; increment counter for bigram
        
        ; go back to loop, if more tokens left
        (if (< i count)
            (loop (+ i 1)))))))


;;; load-file (filename)
;;; load text from file into a string variable and return it
(define load-file
  (lambda (name)
    (call-with-input-file name
      (lambda (p)
        (read-string (file-size name) p)))))


(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (equal? (car l) o) i
              (loop (+ i 1) (cdr l)))))))


(define gen-distributional-vs
  (lambda (columntokens)
    (let* ([wordpositions (hash-table-copy types)]
           [columncount   (length columntokens)]
           [vs            (make-vector 0)])
      
      ; initialize vectors
      (let loop ([i 0])
        (set! vs (vector-append vs (vector (make-vector (* columncount 2) 0.0))))
        (if (< i (- tokencount 1))
            (loop (+ i 1))))
      
      (let ([counter 0])
        (hash-table-for-each wordpositions
                             (lambda (key value)
                               ; remember localy the total freq of the token
                               (let ([tokenfreq value])
                                 ; store position for token in hash-table instead of freq.
                                 (hash-table-put! wordpositions key counter)
                                 ; set the freq of each cooccurence in the vector
                                 (for-each (lambda (bigram)
                                             ; set the cell of left and right token to rel. freq.
                                             (if (member (cadr bigram) columntokens)
                                                 (vector-set! (vector-ref vs counter)
                                                              (list-position (cadr bigram) columntokens)
                                                              (/ (hash-table-get bigrams bigram 0.0)
                                                                 (hash-table-get types key 0.0)))))
                                           (hash-table-get lefttoken key '()))
                                 (for-each (lambda (bigram)
                                             ; set the cell of left and right token to rel. freq.
                                             (if (member (car bigram) columntokens)
                                                 (vector-set! (vector-ref vs counter)
                                                              (+ (list-position (car bigram) columntokens) columncount)
                                                              (/ (hash-table-get bigrams bigram 0.0)
                                                                 (hash-table-get types key 0.0)))))
                                           (hash-table-get righttoken key '())))
                               ; and increment the counter
                               (set! counter (+ counter 1)))))
      (cons wordpositions vs))))





(begin
  (vector-for-each (lambda (i fname)
                     (let ([text (load-file fname)])
                       (string-lowercase! text)
                       ; (add-data (string-tokenize text))
                       (add-data (string-tokenize (pregexp-replace* "[`'-.,!?;:<>()|\"\\]\\[$%/]+" text " ")))
                       ))
                   argv)
  
  (let ([freqprof (sort-by-value types)])
    ; generate 100 most frequent words list
    (set! freqprof (map (lambda (x) (car x))
                        (reverse
                         (list-tail freqprof
                                    (if (> (length freqprof) 50)
                                        (abs (- 50 (length freqprof)))
                                        (length freqprof))))))
    ;(printf "key\t")
    ; print the top 100 words from the decreasing frequency profile
    ;(for-each (lambda (x) (printf "~a\t" x)) freqprof)
    ;(newline)

    ; generate the vector space
    (let* ([vs (gen-distributional-vs freqprof)])
;      (hash-table-for-each (car vs) (lambda (key val)
;                                      (printf "~a\t" key)
;                                      (vector-for-each (lambda (i x)
;                                                         (printf "~a\t" x))
;                                                       (vector-ref (cdr vs) val))
;                                      (newline)))

      (let ([res (soft-k-means (cdr vs) 2)])
        (hash-table-for-each (car vs)
                             (lambda (key value)
                               (printf "~a\t~a\n" key (vector-ref res value))))))))
