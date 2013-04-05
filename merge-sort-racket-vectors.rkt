#lang racket
;; using Racket's internal vector functions. Faster than Structs.

(require racket/unsafe/ops)

(define (merge-sort v)
  (define (sv-merge-sort sv)
    (if (= 1 (unsafe-vector-length sv))
        sv
        (let ([first-half (make-vector (+ 1 (- (quotient (unsafe-vector-length sv) 2) 1)))]
              [second-half (make-vector (+ 1 (- (- (unsafe-vector-length sv) 1) (quotient (unsafe-vector-length sv) 2)) ))])
          (vector-copy! first-half 0 sv 0 (+ 1 (- (quotient (vector-length sv) 2) 1)))
          (vector-copy! second-half 0 sv (quotient (unsafe-vector-length sv) 2) (+ 1 (- (unsafe-vector-length sv) 1)))
          (merge-vectors
           (sv-merge-sort first-half)
           (sv-merge-sort second-half)))))
  (if (= 0 (unsafe-vector-length v))
      v
     (sv-merge-sort v)))
         
(define (merge-vectors a b)
  (when (= (unsafe-vector-length a) 0) b)
  (when (= (unsafe-vector-length b) 0) a)
  (let ([acc (make-vector (+ (unsafe-vector-length a) (unsafe-vector-length b)))])
    (let loop ([ai 0] [bi 0] [acci 0])
      (cond [(and (= ai (unsafe-vector-length a)) (= bi (unsafe-vector-length b)))
             acc]
          [(= ai (unsafe-vector-length a))
           (unsafe-vector-set! acc acci (unsafe-vector-ref b bi))
           (loop ai (+ bi 1) (+ acci 1))]
          [(= bi (unsafe-vector-length b))
           (unsafe-vector-set! acc acci (unsafe-vector-ref a ai))
           (loop (+ ai 1) bi (+ acci 1))]
          [(< (unsafe-vector-ref a ai) (unsafe-vector-ref b bi))
           (unsafe-vector-set! acc acci (unsafe-vector-ref a ai))
           (loop (+ ai 1) bi (+ acci 1))]
          [else
           (unsafe-vector-set! acc acci (unsafe-vector-ref b bi))
           (loop ai (+ bi 1) (+ acci 1))]))))

;; Logging + Profiling
;; ================================================================
(define (random-vector l)
  (for/vector ([i (in-range 0 l)])
    (random l)))

(define (profile-for-n n)
  (let ([v  (random-vector n)])
    (displayln (current-memory-use))
    (collect-garbage) 
    (let-values ([(_ cpu-ms real-ms gc-ms) (time-apply merge-sort (list v))])
      (list cpu-ms real-ms gc-ms))))

(define (our-profile c n)
  (for/list ([i (in-range 1 n)])
    (let ([m (expt 2 i)]) 
      (match-let ([(list cpu-ms real-ms gc-ms)
                   (profile-for-n m)])
        (list m cpu-ms real-ms gc-ms (* 1.0 (/ cpu-ms m)))))))

;'((2 11 13 0 5.5)
;  (4 9 10 0 2.25)
;  (8 9 11 0 1.125)
;  (16 13 14 0 0.8125)
;  (32 9 11 0 0.28125)
;  (64 13 15 0 0.203125)
;  (128 10 12 0 0.078125)
;  (256 10 11 0 0.0390625)
;  (512 15 17 0 0.029296875)
;  (1024 10 12 0 0.009765625)
;  (2048 11 12 0 0.00537109375)
;  (4096 13 14 0 0.003173828125)
;  (8192 15 16 0 0.0018310546875)
;  (16384 31 33 0 0.00189208984375)
;  (32768 41 43 0 0.001251220703125)
;  (65536 70 72 0 0.001068115234375)
;  (131072 126 128 0 0.0009613037109375)
;  (262144 972 976 690 0.0037078857421875)
;  (524288 1180 1190 701 0.00225067138671875)
;  (1048576 1744 1758 773 0.0016632080078125))