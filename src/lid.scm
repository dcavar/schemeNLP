":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: lid.ss
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
;;; 1. loads language models (trigraph statistics)
;;; 2. loads a text and creates a trigraph model
;;; 3. calculates the distance between the trigraph model
;;;    of the unknow text and the trained models
;;; 4. returns the language ID, which corresponds to
;;;    a part of the filename of the closest model
;;;
;;; If the command line parameters contain more than one text file,
;;; the above steps (2.-3.) are performed for each file.
;;;
;;; Usage:
;;; mzscheme -r lid.ss test1.txt test2.txt ...
;;; ----------------------------------------------------

;;; all required libraries and functions
(require (lib "vector-lib.ss" "srfi" "43")) ; for vector-for-each
(require (lib "list.ss"))                   ; for sort
(require (lib "list.ss"       "srfi" "1" ))
(require (lib "serialize.ss"))              ; for serialize
(require (lib "file.ss"))                   ; for fold-files
(require (lib "pregexp.ss"))  ; for Perl compatible regular expressions

;;; Global variables
(define models (make-vector 0)) ; initial vector for language models


;;; load-model
(define load-model
  (lambda (fname)
    (let* ([data  (read (open-input-file fname))]
           [model (deserialize data)]
           [lang  (path->string fname)])
      (printf "Deserialized file: ~a  model for language: ~a\n" lang (substring lang 5 (- (string-length lang) 3)))
      (set! models (vector-append models (vector (list (substring lang 5 (- (string-length lang) 3)) model)))))))


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
(define relativize!
  (lambda (table total)
    (hash-table-for-each table (lambda (key value)
                                 (hash-table-put! table key (/ value total))))))


;;; check-lang
;;; <- string text
;;; -> string id of language
;;; ----------------------------------------------------
(define check-lang
  (lambda (text)
    (let* ([trigramcount (- (string-length text) 2)]
           [trigrams     (make-hash-table 'equal)]
           [countmodels  (vector-length models)]
           [distances (make-vector countmodels)])

      ; create trigraph model of current text
      (let loop ([i 0])
        (let* ([token (substring text i (+ i 3))]
               [value (hash-table-get trigrams token 0.0)])
          (hash-table-put! trigrams token (+ value 1)))
        (if (< i (- trigramcount 1))
            (loop (+ i 1))))
      (relativize! trigrams trigramcount)

      ; calculate distance to each model
      (let modelloop ([j 0])
        (let ([curmodel (cadr (vector-ref models j))]
              [distance 0.0])
          (hash-table-for-each
           trigrams
           (lambda (key value)
             (let ([val (hash-table-get curmodel key -1.0)])
               (if (= val -1.0)
                   (set! distance (+ distance 1))
                   (set! distance (+ distance (abs (- value val))))))))
          (vector-set! distances j distance))
        (if (< j (- countmodels 1))
            (modelloop (+ j 1))))
      (printf "~a\n" distances)

      ; find the smallest distance
      (let* ([counter -1]
             [distl   (map (lambda (a)
                             (set! counter (+ counter 1))
                             (list a counter))
                           (vector->list distances))]
             [smallest (car (sort distl (lambda (a b)
                                               (< (car a) (car b)))))])
        (car (vector-ref models (cadr smallest)))))))


; main procedures
(begin
  ; find language models in the file system (current working directory)
  (for-each (lambda (a)
              (if (pregexp-match "lang-.+\\.ss" (path->string (file-name-from-path a)))
                  (load-model a)))
            (directory-list))
  (printf "Loaded ~a language models.\n" (vector-length models))
  (vector-for-each (lambda (i fname)
                     (printf "File ~a has language ID ~a\n" fname (check-lang (load-file fname))))
                   argv))
