;;; ----------------------------------------------------
;;; Filename: k-means.ss
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
;;; WARNING:
;;; This is an ALPHA HACK!
;;; Please report any problems with the code!
;;;
;;; Usage:
;;; (load "k-means.ss")
;;; ----------------------------------------------------


(require (lib "list.ss"))
(require (lib "vector-lib.ss" "srfi" "43"))
(load "vector-extra.ss")


(define soft-k-means
  (lambda (vectorspace k)
    ; initialize by setting k to most distant or random
    (let* ([centroids    (make-vector k)]
           [centroidhash (make-hash-table 'equal)]
           [error        (make-vector k)]
           [countvec     (vector-length vectorspace)]
           [assign       (make-vector countvec -1)]
           [change       #t]
           [priorerror   0])
      ; initialize the centroids
      ;(printf "Assigned:\n~a\n" assign)
      (initialize! centroids centroidhash k error assign countvec vectorspace)
      ;(printf "Assigned:\n~a\n" assign)
      (set! priorerror (vector-sum error))
      
      ; assign vectors, recalculate centroid, recalculate error for centroid
      (let optimize ()
        (let ([currentstate (vector-copy assign)])
          (set! change #f)
          ; loop over assign: check whether still the same
          (let aloop ([n 0])
            
            (let* ([curvec      (vector-ref vectorspace n)]
                   [assigned    (vector-ref assign n)]
                   [distance    (euclidean-distance curvec (vector-ref centroids assigned))]
                   [tmpdistance 0]
                   [from        assigned]
                   [to          -1])
              (let vloop ([p 0])
                ; get the distance for the centroid
                (set! tmpdistance (euclidean-distance curvec (vector-ref centroids p)))
                ; if smaller
                (if (< tmpdistance distance)
                    (begin
                      ; remember the new distance
                      (set! distance tmpdistance)
                      ; remember the assignment
                      (vector-set! assign n p)
                      (set! to p)))
                (if (< p (- k 1))
                    (vloop (+ p 1))))
              (if (> to -1)
                  (let* ([assigned (vector-ref assign n)]
                         [centvec  (vector-ref centroids assigned)])
                    ; recalculate the centroid to which vec was assigned
                    (vector-set! centroids assigned (get-centroid centvec curvec))
                    ; keep track of which vector is in this centroid
                    (let ([val (hash-table-get centroidhash assigned '())])
                      (unless (member n val)
                        (hash-table-put! centroidhash assigned (concatenate (list val (list n))))
                        ; recalculate the error
                        (let ([er 0.0])
                          (for-each (lambda (x)
                                      (set! er (+ er (euclidean-distance centvec (vector-ref vectorspace x)))))
                                    (hash-table-get centroidhash assigned '()))
                          (vector-set! error assigned er))))
                    ; recalculate the centroid where it was
                    (let ([val (hash-table-get centroidhash from '())]
                          [er  0.0])
                      (hash-table-put! centroidhash from (list-remove val n))
                      ; take initial vec from list and make it the centroid
                      (vector-set! centroids from (vector-copy (vector-ref vectorspace (car val))))
                      ; recalc centroid
                      (for-each (lambda (x)
                                  (vector-set! centroids from (get-centroid
                                                               (vector-ref vectorspace x)
                                                               (vector-ref centroids from))))
                                (cdr val))
                      (for-each (lambda (x)
                                      (set! er (+ er (euclidean-distance
                                                      (vector-ref centroids from)
                                                      (vector-ref vectorspace x)))))
                                    val)
                      (vector-set! error from er))))
            
            (if (< n (- (vector-length assign) 1))
                (aloop (+ n 1))))
          ; if error did not improve, set change to #f
          (let ([errorsum (vector-sum error)])
            (if (< errorsum priorerror)
                (begin
                  (set! priorerror errorsum)
                  (set! change #t))
                (begin
                  (set! change #f)
                  (set! assign currentstate))))) ; restore the prior state and return
        (if change
            (optimize)))
      assign))))


(define initialize!
  (lambda (centroids centroidhash k error assign countvec vectorspace)
    (vector-for-each
     (lambda (i vec)
       (let ([num (random countvec)])
         ; repeat until a vector is found that does not belong to a cluster
         (unless (> (vector-ref assign num) -1)
           (let randloop ()
             (set! num (random countvec))
             (if (> (vector-ref assign num) -1)
                 (randloop))))
         ; set the assignment of the vector
         (vector-set! assign num i)
         ; copy the vector into the centroid
         (vector-set! centroids i (vector-copy (vector-ref vectorspace num)))
         ; remember which vector for this centroid
         (hash-table-put! centroidhash i (list num))))
     centroids)
    
    ; assign other vectors
    (vector-for-each
     (lambda (i a)
       (if (= a -1)
           ; for each centroid, check which one is closest
           (let* ([curvec      (vector-ref vectorspace i)]
                  [distance    (euclidean-distance curvec (vector-ref centroids 0))]
                  [tmpdistance 0])
             (vector-set! assign i 0) ; set the vector assignment for 0
             (let vloop ([p 1])
               ; get the distance for the centroid
               (set! tmpdistance (euclidean-distance curvec (vector-ref centroids p)))
               ; if smaller
               (if (< tmpdistance distance)
                   (begin
                     ; remember the new distance
                     (set! distance tmpdistance)
                     ; remember the assignment
                     (vector-set! assign i p)))
               (if (< p (- k 1))
                   (vloop (+ p 1))))
             (let* ([assigned (vector-ref assign i)]
                    [centvec  (vector-ref centroids assigned)])
               ; recalculate the centroid to which vec was assigned
               (vector-set! centroids assigned (get-centroid centvec curvec))
               ; keep track of which vector is in this centroid
               (let ([val (hash-table-get centroidhash assigned '())])
                 (unless (member i val)
                   (hash-table-put! centroidhash assigned (concatenate (list val (list i))))
                   ; recalculate the error
                   (let ([er 0.0])
                     (for-each (lambda (x)
                                 (set! er (+ er (euclidean-distance centvec (vector-ref vectorspace x)))))
                               (hash-table-get centroidhash assigned '()))
                     (vector-set! error assigned er))))))))
     assign)))
