;;; ----------------------------------------------------
;;; Filename: list-extra.ss
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
;;; Functions:
;;; sublist (list from-integer to-integer)
;;; Returns a sublist from a list, including the from index,
;;; excluding the to index.
;;; This function is safe for the indices being out of range.
;;;
;;; Usage:
;;; (load "list-extra.ss")
;;; ----------------------------------------------------


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

