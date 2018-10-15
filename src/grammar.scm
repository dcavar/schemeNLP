":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: grammar.ss
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
;;; 1. Reads a grammar file from files.
;;; 2. Tokenizes each line and reads in left- and right-hand-side
;;;    into hash-tables.
;;;
;;; If the command line parameters contain more than one text/grammar file,
;;; the above results are accumulated over all the input files into one hash-table.
;;;
;;; The grammar files have one CFG-rule per line:
;;; S -> NP VP
;;;
;;; Comment lines start with a hash-mark #.
;;;
;;; Empty lines are allowed.
;;;
;;; Usage:
;;; mzscheme -r grammar.ss grammar1.txt grammar2.txt ...
;;; ----------------------------------------------------

(require (lib "list.ss"   "srfi" "1"))
(require (lib "string.ss" "srfi" "13"))


(define lhs  (make-hash-table 'equal))
(define rhs  (make-hash-table 'equal))


;;; reading from file line by line
(define fold-lines-in-file
  (lambda (filename proc init . mode)
    (with-input-from-file filename
      (lambda ()
        (apply fold-lines proc init (current-input-port) mode)))))

(define fold-lines
  (lambda (proc init . port+mode)
    (let while ((accum init))
      (let ([line (apply read-line port+mode)])
        (if (eof-object? line) accum
          (while (proc line)))))))


;;; parsing the grammar string
(define parse-rule!
  (lambda (line)
    (if (< 0 (string-length line))
        (unless (eq? (string-ref line 0) #\#)
          (let ([tokens (string-tokenize line)])
            (if (string= (cadr tokens) "->")
                (let* ([lhst (string->symbol (car tokens))]
                       [rhst (map-in-order (lambda (x)
                                    (string->symbol x)) (cddr tokens))]
                       [lhsval (hash-table-get lhs lhst '())]
                       [rhsval (hash-table-get rhs rhst '())])
                  ;
                  (unless (member rhst lhsval)
                    (hash-table-put! lhs lhst (cons rhst lhsval)))
                  ;
                  (unless (member lhst rhsval)
                    (hash-table-put! rhs rhst (cons lhst rhsval))))))))))


;;; loading rules from a grammar file
(define load-grammar
  (lambda (filename)
    (fold-lines-in-file filename
                        (lambda (line)
                          (parse-rule! (string-trim-both line)))
                        1)))

