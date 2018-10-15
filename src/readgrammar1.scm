":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: readgrammar1.ss
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
;;; mzscheme -r readgrammar1.ss grammar1.txt grammar2.txt ...
;;; ----------------------------------------------------
(require (lib "string.ss"     "srfi" "13"))


(define lhs (make-hash-table 'equal))
(define rhs (make-hash-table 'equal))

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
                       [rhst (map (lambda (x)
                                    (string->symbol x)) (cddr tokens))]
                       [lhstbl (hash-table-get lhs lhst (make-hash-table 'equal))]
                       [lhsval (hash-table-get lhstbl rhst 0.0)]
                       [rhstbl (hash-table-get rhs rhst (make-hash-table 'equal))]
                       [rhsval (hash-table-get rhstbl lhst 0.0)])
                  ;
                  (if (= lhsval 0.0)
                      (hash-table-put! lhstbl rhst 1.0)
                      (hash-table-put! lhstbl rhst (+ rhsval 1)))
                  ;
                  (if (= rhsval 0.0)
                      (hash-table-put! rhstbl lhst 1.0)
                      (hash-table-put! rhstbl lhst (+ lhsval 1)))
                  (hash-table-put! lhs lhst lhstbl)
                  (hash-table-put! rhs rhst rhstbl))))))))


;(let loop ([i 0])
;  (unless (>= i (vector-length argv))
;    (fold-lines-in-file (vector-ref argv i)
;                        (lambda (line)
;                          (parse-rule! (string-trim-both line)))
;                        1)
;    (hash-table-for-each lhs
;                         (lambda (key value)
;                           (hash-table-for-each value (lambda (vkey vval) 
;                                                        (printf "~a\t->\t~a\n" key vkey)))))
;    (loop (+ i 1))))
