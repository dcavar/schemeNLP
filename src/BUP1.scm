":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: BUP1.ss
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
;;; 1. Reads in a grammar from files named grammar*.txt.
;;; 2. Parses the words given as command line arguments.
;;; 3. Returns true or false (parsed or not parsed).
;;;
;;; Usage:
;;; mzscheme -r BUP1.ss John kissed Mary
;;; ----------------------------------------------------

(require (lib "list.ss"))
(require (lib "list.ss"   "srfi" "1" ))
(require (lib "string.ss" "srfi" "13"))
(require (lib "hash.ss"   "srfi" "69"))

(load "grammar.ss")


(define sublist
  (lambda (mlist from to)
    (let ([res     '()]
          [counter 0])
      (for-each (lambda (x)
                  (if (and (<= from counter) (< counter to))
                      (if (> (length res) 0)
                          (set! res (append res (list x)))
                          (set! res (list x))))
                  (set! counter (+ counter 1)))
                mlist)
      res)))



(define buparse
  (lambda (tokens goal agenda)
    (printf "Looking at: ~a\n" tokens)
    (let window ([w (length tokens)])
      (let loop ([i 0])
        ;(printf "~a  pos: ~a  window: ~a\n" (sublist tokens i (+ i w)) i w)
        ;(printf "Rest: ~a - ~a\n" (take tokens i) (take-right tokens (- (length tokens) (+ i w))))
        (let ([x (sublist tokens i (+ i w))])
          (let ([vals  (hash-table-get rhs x '())]
                [parse '()])
            (for-each (lambda (y)
                        (set! parse (concatenate (list (take tokens i)
                                                       (list y)
                                                       (take-right tokens (- (length tokens) (+ i w))))))
                        (unless (member parse agenda)
                          (set! agenda (append agenda (list parse)))))
                      vals)))
        (unless (>= (+ i w) (length tokens))
          (loop (+ i 1))))
      (unless (<= w 1)
        (window (- w 1))))
    ;    (printf "Agenda:\n")
    ;    (for-each (lambda (x)
    ;                (printf "~a\n" x))
    ;              agenda)
    
    (if (member goal agenda)
        #t ; if goal in agenda, success
        (if (> (length agenda) 0) ; else
            (buparse (car agenda) goal (cdr agenda)) ; if some path on agenda
            #f)))) ; else fail




(load-grammar "grammar.txt")
; (hash-table-for-each rhs (lambda (key value) (printf "~a\t~a\n" key value)))
(if (< 0 (vector-length argv))
    (if (buparse (map-in-order (lambda (x)
                                 (string->symbol x))
                               (vector->list argv))
                 (list 'S)
                 '())
        (printf "Success!\n")
        (printf "Fail!\n")))
; (buparse (list 'John 'loves 'Mary) (list 'S) '())



