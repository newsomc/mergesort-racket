#lang racket
(require racket/unsafe/ops)

(define (merge-sort v)
  (define (sv-merge-sort sv)
    (cond 
    [(= 1 (sub-vector-length sv)) sv]
          [(= 2 (sub-vector-length sv)) 
           (let ([a (sub-vector-ref sv 0)]
                 [b (sub-vector-ref sv 1)])
             (if (>= a b)
                 (begin
                   (sub-vector-set-multiple! sv b a)
                   sv)
                 sv))]
          [(= 3 (sub-vector-length sv))
           (let ([a (sub-vector-ref sv 0)]
                 [b (sub-vector-ref sv 1)]
                 [c (sub-vector-ref sv 2)])
             (cond [(<= a b c)
                    (sub-vector-set-multiple! sv a b c)]
                   [(<= a c b)
                    (sub-vector-set-multiple! sv a c b)]
                   [(<= b a c)
                    (sub-vector-set-multiple! sv b a c)]
                   [(<= b c a)
                    (sub-vector-set-multiple! sv b c a)]
                   [(<= c a b)
                    (sub-vector-set-multiple! sv c a b)]
                   [(<= c b a)
                    (sub-vector-set-multiple! sv c b a)])
             sv)]            
          [else (let ([first-half (sub-vector->sub-vector sv 0 (- (quotient (sub-vector-length sv) 2) 1))]
                      [second-half (sub-vector->sub-vector sv (quotient (sub-vector-length sv) 2) (- (sub-vector-length sv) 1))])
                  (merge-sub-vectors
                   (sv-merge-sort first-half)
                   (sv-merge-sort second-half)))]))
  (if (= 0 (unsafe-vector-length v))
      v
      (sub-vector-vec (sv-merge-sort (vector->sub-vector v 0 (- (unsafe-vector-length v) 1))))))
         
(define (merge-sub-vectors a b)
  (when (= (sub-vector-length a) 0) b)
  (when (= (sub-vector-length b) 0) a)
  (let ([acc (make-vector (+ (sub-vector-length a) (sub-vector-length b)))])
    (let loop ([ai 0] [bi 0] [acci 0])
      (cond 
          [(and (= ai (sub-vector-length a)) (= bi (sub-vector-length b)))
             (vector->sub-vector acc 0 (- (unsafe-vector-length acc) 1))]
          [(= ai (sub-vector-length a))
           (unsafe-vector-set! acc acci (sub-vector-ref b bi))
           (loop ai (+ bi 1) (+ acci 1))]
          [(= bi (sub-vector-length b))
           (unsafe-vector-set! acc acci (sub-vector-ref a ai))
           (loop (+ ai 1) bi (+ acci 1))]
          [(< (sub-vector-ref a ai) (sub-vector-ref b bi))
           (unsafe-vector-set! acc acci (sub-vector-ref a ai))
           (loop (+ ai 1) bi (+ acci 1))]
          [else
           (unsafe-vector-set! acc acci (sub-vector-ref b bi))
           (loop ai (+ bi 1) (+ acci 1))]))))

;; Subvector datatype. Vectors are represented as 1. the original vector
;; and 2. the start and end positions of each sub vector. 
(struct sub-vector (vec start-pos end-pos) #:mutable #:transparent)

;; Convert a vector to a subvector. Basically, get the correct 
;; start/end indeces for each subvec. 
;; Using Macros to inline the following conversions.
(define-syntax-rule (vector->sub-vector v vi vj)
  (sub-vector v vi vj))

(define-syntax-rule (sub-vector->sub-vector sv svi svj)
  (sub-vector (unsafe-struct-ref sv 0) (+ svi (unsafe-struct-ref sv 1)) (+ svj (unsafe-struct-ref sv 1))))

(define-syntax-rule (sub-vector-ref sv i)
  (unsafe-vector-ref (unsafe-struct-ref sv 0) (+ (unsafe-struct-ref sv 1) i)))

(define-syntax-rule (sub-vector-set! sv i val)
  (vector-set! (unsafe-struct-ref sv 0) (+ (unsafe-struct-ref sv 1) i) val))

;; Used for special casing (currently up to 3). 
;; Simply set multiple values in a vector. 
(define (sub-vector-set-multiple! sv . vals)
  (for ([i (in-range 0 (length vals))] 
        [e (in-list vals)])
    (sub-vector-set! sv i e))
  sv)
  
;; No error checking. We found that this made things more expensive. 
;; Therefore, the correct indeces must be provided 100% of the time.
(define (sub-vector-length sv)
  (+ 1 (- (unsafe-struct-ref sv 2) (unsafe-struct-ref sv 1))))

;; Print a subvector
;; ==================================================================================
(define (display-sub-vector sv)
  (for/list ([i (in-range (unsafe-struct-ref sv 1) (+ 1 (unsafe-struct-ref sv 2)))])
    (unsafe-vector-ref (unsafe-struct-ref sv 0) i))) 


;; Logging and benchmarking
;; ==================================================================================
(define (random-vector l)
  (for/vector ([i (in-range 0 l)])
    (random l)))

(define (profile-for-n n)
  (let ([v  (random-vector n)])
    (displayln (current-memory-use))
    (collect-garbage) 
    (let-values ([(_ cpu-ms real-ms gc-ms) (time-apply merge-sort (list v))])
      (list cpu-ms real-ms gc-ms))))

;; Use "-profile" fns at the REPL.
(define (our-profile c n)
  (for/list ([i (in-range 1 n)])
    (let ([m (+ 1 (expt 2 i))]) 
      (match-let ([(list cpu-ms real-ms gc-ms)
                   (profile-for-n m)])
        (list m cpu-ms real-ms gc-ms (* 1.0 (/ cpu-ms m)))))))

;; Test Racket's sorting. More simplistic logging here.
(require rnrs/sorting-6)

(define (their-profile c n)
  (for/list ([i (in-range 1 n)])
    (let* ([m (expt 2 i)] 
           [cpums (their-profile-for-n m)])
      (list m cpums (* 1.0 (/ cpums m))))))

(define (their-profile-for-n n)
  (let ([v  (random-vector n)])
    (collect-garbage) 
    (let-values ([(_ cpu-ms __ ____) (time-apply vector-sort (list < v))])
      cpu-ms)))

; RESULTS
; ==================================================================================

; RACKET'S INTERNAL VECTOR SORT
; ==================================================================================
;'((2 18 9.0)
;  (4 11 2.75)
;  (8 14 1.75)
;  (16 10 0.625)
;  (32 10 0.3125)
;  (64 14 0.21875)
;  (128 13 0.1015625)
;  (256 17 0.06640625)
;  (512 40 0.078125)
;  (1024 57 0.0556640625)
;  (2048 97 0.04736328125)
;  (4096 195 0.047607421875)
;  (8192 378 0.046142578125)
;  (16384 786 0.0479736328125)
;  (32768 1655 0.050506591796875)
;  (65536 3535 0.0539398193359375)
;  (131072 8158 0.0622406005859375)
;  (262144 16349 0.062366485595703125)
;  (524288 33554 0.06399917602539062)
;  (1048576 69635 0.06640911102294922)
;  (2097152 147580 0.07037162780761719))

; NO SPEC CASES
; ==================================================================================
;'((2 11 15 0 5.5)
;  (4 11 15 0 2.75)
;  (8 15 19 0 1.875)
;  (16 17 21 0 1.0625)
;  (32 18 21 0 0.5625)
;  (64 16 17 0 0.25)
;  (128 19 22 0 0.1484375)
;  (256 20 23 0 0.078125)
;  (512 19 22 0 0.037109375)
;  (1024 14 15 0 0.013671875)
;  (2048 27 29 0 0.01318359375)
;  (4096 13 17 0 0.003173828125)
;  (8192 45 50 0 0.0054931640625)
;  (16384 44 48 0 0.002685546875)
;  (32768 63 68 0 0.001922607421875)
;  (65536 100 104 0 0.00152587890625)
;  (131072 177 182 0 0.00135040283203125)
;  (262144 1440 1454 1078 0.0054931640625)
;  (524288 1806 1820 1107 0.003444671630859375)
;  (1048576 2709 2752 1263 0.0025835037231445312))

; WITH SPEC CASE FOR 2
; ==================================================================================
;'((2 19 22 0 9.5)
;  (4 19 23 0 4.75)
;  (8 16 20 0 2.0)
;  (16 10 14 0 0.625)
;  (32 35 41 0 1.09375)
;  (64 16 20 0 0.25)
;  (128 14 18 0 0.109375)
;  (256 18 22 0 0.0703125)
;  (512 18 23 0 0.03515625)
;  (1024 19 23 0 0.0185546875)
;  (2048 20 24 0 0.009765625)
;  (4096 40 45 0 0.009765625)
;  (8192 20 24 0 0.00244140625)
;  (16384 41 44 0 0.00250244140625)
;  (32768 59 71 0 0.001800537109375)
;  (65536 95 99 0 0.0014495849609375)
;  (131072 161 166 0 0.00122833251953125)
;  (262144 304 308 0 0.00115966796875)
;  (524288 1707 1717 1084 0.0032558441162109375)
;  (1048576 2458 2482 1193 0.0023441314697265625))

; WITH SPEC CASE FOR 2 AND 3
; ==================================================================================
;'((3 27 41 0 9.0)
;  (5 11 12 0 2.2)
;  (9 11 11 0 1.2222222222222223)
;  (17 10 11 0 0.5882352941176471)
;  (33 14 15 0 0.42424242424242425)
;  (65 11 12 0 0.16923076923076924)
;  (129 11 11 0 0.08527131782945736)
;  (257 10 11 0 0.038910505836575876)
;  (513 10 12 0 0.01949317738791423)
;  (1025 11 11 0 0.010731707317073172)
;  (2049 11 13 0 0.005368472425573451)
;  (4097 14 15 0 0.003417134488650232)
;  (8193 18 18 0 0.0021969974368363236)
;  (16385 27 28 0 0.001647848642050656)
;  (32769 48 49 0 0.0014647990478806189)
;  (65537 78 79 0 0.0011901673863619025)
;  (131073 142 142 0 0.0010833657580127105)
;  (262145 277 279 0 0.0010566671117129833)
;  (524289 1187 1189 605 0.00226401850887583)
;  (1048577 1840 1845 651 0.0017547590687188447))
