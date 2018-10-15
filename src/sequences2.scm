(vector 0.1 0.4 #\a "Hello" (vector 3 7 #\t) )
(define parameters (make-vector 4))
parameters
(vector? parameters)
(vector-set! parameters 3 #\a)
(vector-ref parameters 2)
(vector-ref parameters 3)
