;;; ----------------------------------------------------
;;; Filename: vector-extras.ss
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
;;; These functions offer vector manipulation and vector
;;; distance calculations.
;;;
;;; WARNING:
;;; This is an ALPHA HACK!
;;; Please report any problems with the code!
;;;
;;; Usage:
;;; (load "vector-extras.ss")
;;; ----------------------------------------------------

(require (lib "list.ss"       "srfi" "1" ))
(require (lib "vector-lib.ss" "srfi" "43"))


; get-centroid (v1 v2)
; returns the centroid between two vectors
; assuming the vectors are of the same size
(define get-centroid
  (lambda (v1 v2)
    (vector-map (lambda (i x)
                  (/ (+ (vector-ref v1 i) x) 2))
                v2)))


; vector-min (vec)
; returns the index position of the smallest number in the vector
(define vector-min
  (lambda (vec)
    (let ([pos      -1]
          [smallest (vector-ref vec 0)])
      (vector-for-each
       (lambda (i a)
         (if (< a smallest)
             (begin
               (set! pos      i)
               (set! smallest a))))
       vec)
      pos)))


; vector-max (vec)
; returns the index position of the highest number in the vector
(define vector-max
  (lambda (vec)
    (let ([pos     -1]
          [highest (vector-ref vec 0)])
      (vector-for-each
       (lambda (i a)
         (if (> a highest)
             (begin
               (set! pos      i)
               (set! highest a))))
       vec)
      pos)))

; vector-remove (vec elem)
; return the vector without element
(define vector-remove
  (lambda (vec elem)
    (let ([tvec (make-vector 0)])
      (vector-for-each
       (lambda (i e)
         (unless (= e elem)
           (set! tvec (vector-append tvec (vector e)))))
       vec)
      tvec)))


; list-remove (list elem)
; return the vector without element
(define list-remove
  (lambda (vec elem)
    (let ([tvec '()])
      (for-each
       (lambda (e)
         (unless (= e elem)
           (set! tvec (concatenate (list tvec (list e))))))
       vec)
      tvec)))


; vector-sum (vec)
; return the sum over all elements in a vector
(define vector-sum
  (lambda (vec)
    (let ([res 0])
      (vector-for-each
       (lambda (i e)
         (set! res (+ res e)))
       vec)
      res)))


; euclidean-distance (v1 v2)
; returns the Euclidean distance between two vectors
; assuming the vectors are of equal length
; sqrt \sum_{i = 0}^{n} \left( p_i - q_i \right)^2
(define euclidean-distance
  (lambda (v1 v2)
    (let ([vectorlength (vector-length v1)]
          [distance     0.0])
      ; vectors are not of equal length!
      ; we could append 0 for the missing dimensions
      ;(if (<> vectorlength (vector-length v1))
      ;    #f)
      (let loop ((i 0))
        (unless (>= i vectorlength)
          (set! distance (+ (expt (- (vector-ref v1 i) (vector-ref v2 i)) 2) distance))
          (loop (+ i 1))))
      (sqrt distance))))


; sum of scalar product divided by the multiplications of the square of dot products
(define cosine-similarity
  (lambda (v1 v2)
    (let ((spv1 (scalar-product v1))
          (spv2 (scalar-product v2))
          (sp   0.0)
          (vlen (vector-length v1)))
      (let loop ((i 0))
        (unless (>= i vlen)
          (set! sp (+ sp (* (vector-ref v1 i) (vector-ref v2 i))))
          (loop (+ i 1))))
      (/ sp (* spv1 spv2)))))


(define scalar-product
  (lambda (v)
    (let ((vlen (vector-length v))
          (scalar 0.0))
      (let loop ((i 0))
        (unless (>= i vlen)
          (set! scalar (+ scalar (expt (vector-ref v i) 2)))
          (loop (+ i 1))))
      (sqrt scalar))))
