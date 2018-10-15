":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: Charty.ss
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
;;; Usage:
;;; mzscheme -r Charty.ss John kissed Mary
;;; ----------------------------------------------------

(load "list-extra.ss")
(load "grammar2.ss")


;;; global variable for the chart
(define chart '())


(define inactive-edge
  (lambda (edge)
    (if (>= (caddr edge) (length (list-ref edge 4)))
        #t
        #f)))


(define active-edge
  (lambda (edge)
    (not (inactive-edge edge))))


;;; rule-invocation
;;; find complete edges and add rules that have the LHS symbol
;;; as the first RHS symbol
(define rule-invocation!
  (lambda (start)
    (printf "Rule invocation:\n")
    (for-each ; edge on chart starting from start
     (lambda (edge)
       (if (inactive-edge edge) ; if edge not active
           (let ([rhss (hash-table-get rhsl (list-ref edge 3) '())])
             (for-each ; for each RHS with LHS as initial symbol
              (lambda (x)
                (for-each ; for each LHS with this RHS
                 (lambda (y) ; create new edge
                   (let ([newedge (list (car edge) (cadr edge) 1 y x)])
                     (printf "Based on edge: ~a\n" edge)
                     (printf "Adding edge:   ~a\n" newedge)
                     (unless (member chart newedge) ; if not on chart, append to chart
                       (set! chart (append chart (list newedge))))))
                 (hash-table-get rhs x '())))
              rhss))))
     (drop chart start))))


;;; fundamental-rule
;;; find active edges and axpand them with inactive, i.e.
;;; the first symbol after the dot as their LHS symbol
(define fundamental-rule!
  (lambda ()
    (printf "Fundamental rule:\n")
    (for-each ; edge on chart
     (lambda (edge)
       (if (active-edge edge)
           (let ([expectation (list-ref (list-ref edge 4) (caddr edge))])
             (for-each ; edge on chart
              (lambda (oe)
                (if (inactive-edge oe)
                    (if (and (eq? expectation (list-ref oe 3))
                             (= (cadr edge) (car oe)))
                        (begin
                          (let ([newedge (list (car edge)
                                               (cadr oe)
                                               (+ (caddr edge) 1)
                                               (list-ref edge 3)
                                               (list-ref edge 4))])
                            (unless (member newedge chart)
                              (printf "These edges fit:\n~a\n~a\n" edge oe)
                              (printf "Adding edge: ~a\n" newedge)
                              (set! chart (append chart (list newedge)))))))))
              chart))))
     chart)))


(define parse
  (lambda (tokens)
    (printf "Initialize the chart from tokens: ~a\n" tokens)    
    
    (let window ([w (length tokens)])
      (let iloop ([i 0])
        (let ([x (sublist tokens i (+ i w))])
          (let ([vals  (hash-table-get rhs x '())]
                [parse '()])
            (for-each (lambda (y)
                        ; create edge with span
                        (let ([edge (list i (+ i w) w y x)])
                          (unless (member edge chart)
                            (set! chart (append chart (list edge))))))
                      vals)))
        (unless (>= (+ i w) (length tokens))
          (iloop (+ i 1))))
      (unless (<= w 1)
        (window (- w 1))))
    ; apply rule invocation, fundamental rule, until nothing more possible
    (let ([start  0]
          [oldlen (length chart)])
      (let loop ()
        (rule-invocation! start)
        (fundamental-rule!)
        ; something changed on the chart
        (if (> (length chart) oldlen)
            (begin
              (set! start   oldlen)
              (set! oldlen  (length chart))
              (loop)))))))



;;; -----------------------------
(load-grammar "grammar.txt")
(if (< 0 (vector-length argv))
    (begin
      (parse (map-in-order (lambda (x)
                             (string->symbol x))
                           (vector->list argv)))
      (printf "Final chart:\n")
      (for-each (lambda (z)
                  (printf "~a\n" z))
                chart)))



