":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: trigraph.ss
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
;;; 3. The absolute counts are relativized.
;;; 4. The hash-table is dumped (serialized) to stdout.
;;;
;;; If the command line parameters contain more than one text file,
;;; the above results are accumulated over all the input text files.
;;;
;;; Usage:
;;; mzscheme -r trigraph.ss test1.txt test2.txt ... > language.ss
;;; ----------------------------------------------------

;;; all required libraries and functions
(require (lib "vector-lib.ss" "srfi" "43")) ; for vector-for-each
(require (lib "list.ss"))                   ; for sort
(require (lib "serialize.ss"))              ; for serialize

;;; Global variables
(define trigramcount 0.0)                      ; counter of total number tokens
(define trigrams     (make-hash-table 'equal)) ; hash-table for tokens and counts

;;; add-trigrams
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
          (hash-table-put! trigrams token (+ value 1.0)))
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


;;; relativize
;;; <- hash-table with keys and their absolute count
;;; <- total number of tokens
;;; ----------------------------------------------------
;;; side effect: overwrites the value with the relativized count.
(define relativize
  (lambda (table total)
    (hash-table-for-each table (lambda (key value)
                                 (hash-table-put! table key (/ value total))))))


;;; ----------------------------------------------------
;;; main steps
(begin 
  (vector-for-each (lambda (i fname)
                     (add-trigrams (load-file fname)))
                   argv)
  (relativize trigrams trigramcount)
  (write (serialize trigrams)))
