":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: mkre.ss
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
;;; 1. The text with stop-words is loaded into memory.
;;; 2. The text is tokenized, i.e. converted into a list of tokens.
;;; 3. The list is converted into a regular expression that matches
;;;    with all tokens.
;;;
;;; Usage:
;;; mzscheme -r mkre.ss english.txt > english.ss
;;; ----------------------------------------------------

(require (lib "vector-lib.ss" "srfi" "43")) ; for vector-for-each
(require (lib "string.ss" "srfi" "13")) ; for string-tokenize
(require (lib "list.ss")) ; for sort


(define (list->re wlist)
  (let ([next (make-hash-table 'equal)]
        [res '()])
    (if (> (length wlist) 0)
        (begin
          (for-each (lambda (a)
                      (let* ([key (make-string 1 (string-ref a 0))]
                             [rest (substring a 1 (string-length a))]
                             [val (hash-table-get next key '())])
                        (if (> (string-length rest) 0)
                            ; place first char as key, rest as val
                            (unless (member rest val)
                              (hash-table-put! next key (append (list rest) val))))))
                    wlist)
          (hash-table-for-each next
                               (lambda (key val)
                                 (if (= (length val) 1)
                                     (set! res (append (list (string-append key (list-ref val 0))) res))
                                     (set! res (append (list (list key (list->re val))) res)))))))
    res))


(define (wordlist->regexp wlist)
  (let ([regexp ""])
    ; find the longest common prefix for a group of words
    
  regexp))


(let ([words (map (lambda (w)
                    (string-append "#"
                                   w
                                   "@"))
                  (string-tokenize
                   (call-with-input-file "german"
                     (lambda (p)
                       (read-string (file-size "german") p)))))])
  (for-each (lambda (a)
              (printf "~a\n" a))
            (cadar (list->re words))))

;(begin
;  (vector-for-each (lambda (i fname)
;                     (let ([words (sort
;                                   (string-tokenize
;                                    (call-with-input-file fname
;                                      (lambda (p)
;                                        (read-string (file-size fname) p))))
;                                   (lambda (a b)
;                                     (string<? a b)))])
;                       ;(printf "~a\n" (list->re words))
;                       (for-each (lambda (a)
;                                   (printf "~a\n" a))
;                                 (list->re words))
;                       ;(display "(define stopwords '")
;                       ;(write text)
;                       ;(display ")")
;                       ;(newline))
;                       ))
;                   argv))
