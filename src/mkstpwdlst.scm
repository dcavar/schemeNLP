":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: mkstpwdlst.ss
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
;;; 3. A list of tokens is displayed, and can be redirected into
;;;    a Scheme source file, see Usage.
;;;
;;; Usage:
;;; mzscheme -r mkstpwdlst.ss english.txt > english.ss
;;; ----------------------------------------------------

(require (lib "vector-lib.ss" "srfi" "43")) ; for vector-for-each
(require (lib "string.ss"     "srfi" "13")) ; for string-tokenize
(require (lib "list.ss")) ; for sort


;;; load-file
;;; <- string filename
;;; -> string file content
;;; ----------------------------------------------------
;;; Load text from file into a string variable and return it.
(define load-file
  (lambda (name)
    (let* ((size (file-size name)))
      (call-with-input-file name
        (lambda (p)
          (read-string size p))))))


(begin
  (vector-for-each (lambda (i fname)
                     (let ([text (sort
                                  (string-tokenize
                                   (load-file fname))
                                  (lambda (a b)
                                    (string<? a b)))])
                       (display "(define stopwords '")
                       (write text)
                       (display ")")
                       (newline)))
                   argv))
