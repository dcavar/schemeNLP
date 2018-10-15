":"; exec mzscheme -r $0 "$@"

;;; ----------------------------------------------------
;;; Filename: Charty3.ss
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
;;; mzscheme -r Charty3.ss John kissed Mary
;;; ----------------------------------------------------

(load "list-extra.ss")
(load "grammar2.ss")


;;; global variable for the chart
(define chart '())


;;; inactive-edge (edge)
;;; true is edge inactive, else false
(define inactive-edge
  (lambda (edge)
    (if (>= (caddr edge) (length (list-ref edge 4)))
        #t
        #f)))


;;; active-edge (edge)
;;; negation of inactive-edge
(define active-edge
  (lambda (edge)
    (not (inactive-edge edge))))


;;; rule-invocation
;;; find complete edges and add rules that have the LHS symbol
;;; as the first RHS symbol
(define rule-invocation!
  (lambda (start)
    (printf "Start: ~a\n" start)
    (for-each ; edge on chart starting from start
     (lambda (edge)
       (if (inactive-edge edge) ; if edge not active
           (let ([rhss (hash-table-get rhsl (list-ref edge 3) '())])
             (for-each ; for each RHS with LHS as initial symbol
              (lambda (x)
                (for-each ; for each LHS with this RHS
                 (lambda (y) ; create new edge
                   (let ([newedge (list (car edge) (cadr edge) 1 y x
                                        (append (list (list-index
                                                       (lambda (el)
                                                         (eq? el edge))
                                                       chart))))])
                     (unless (member chart newedge) ; if not on chart, append to chart
                       (printf "RI Newedge: ~a\n" newedge)
                       (set! chart (append chart (list newedge))))))
                 (hash-table-get rhs x '())))
              rhss))))
     (drop chart start))))


;;; fundamental-rule
;;; find active edges and axpand them with inactive, i.e.
;;; the first symbol after the dot as their LHS symbol
(define fundamental-rule!
  (lambda ()
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
                                               (list-ref edge 4)
                                               (append (list-ref edge 5)
                                                       (list (list-index
                                                              (lambda (el)
                                                                (eq? el oe))
                                                              chart))))])
                            (unless (member newedge chart)
                              (printf "FR Newedge: ~a\n" newedge)
                              (set! chart (append chart (list newedge)))))))))
              chart))))
     chart)))


(define parse
  (lambda (tokens)
    (let window ([w (length tokens)])
      (let iloop ([i 0])
        (let ([x (sublist tokens i (+ i w))])
          (let ([vals  (hash-table-get rhs x '())]
                [parse '()])
            (for-each (lambda (y)
                        ; create edge with span
                        (let ([edge (list i (+ i w) w y x '())])
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
        (printf "Chart-length: ~a\n" (length chart))
        ; something changed on the chart
        (if (> (length chart) oldlen)
            (begin
              (set! start  oldlen)
              (set! oldlen (length chart))
              (loop)))))))



;;; serialize-chart
;;; create lists of complete overspanning edges
(define serialize-chart
  (lambda (chart input)
    (let ([res '()])
      (for-each
       (lambda (edge)
         (let ([end (length input)])
           (if (and (inactive-edge edge) ; inactive edge
                    (= (car edge) 0)     ; from beginning
                    (= (cadr edge) end)) ; till end
               (begin
                 (unless (member (list (edge->list edge)) res)
                   (set! res (append res (list (edge->list edge)))))))))
       (reverse chart))
      res)))


;;; edge->list
;;; converts embedded edges into an embedded list
(define edge->list
  (lambda (edge)
    (let ([str (list (list-ref edge 3))])
      (if (> (length (list-ref edge 5)) 0)
          (for-each
           (lambda (i)
             (set! str (append str (list (edge->list (list-ref chart i))))))
           (list-ref edge 5))
          (set! str (append str (list-ref edge 4))))
      str)))



;;; -----------------------------
(load-grammar "grammar.txt")
(if (< 0 (vector-length argv))
    (let ([input (map-in-order (lambda (x)
                                 (string->symbol x))
                               (vector->list argv))])
      (parse input)
      (printf "Final chart:\n")
      (let ([counter -1])
        (for-each (lambda (z)
                    (set! counter (+ counter 1))
                    (if (inactive-edge z)
                        (printf "~a: ~a\n" counter z)))
                  chart))
      (printf "Parses:\n")
      (let ([counter 0])
        (for-each (lambda (x)
                    (set! counter (+ counter 1))
                    (printf "~a: ~a\n" counter x))
                  (serialize-chart chart input)))))

