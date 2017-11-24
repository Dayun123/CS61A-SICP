;; Cube roots by newtons method, take a guess and the radicand and keep
;; improving upon that guess until it is within an acceptable tolerance
;; of the root.
(define (cube-er guess r)
  (if (good-enough? guess r)
    guess
    (cube-er (improve guess r) r)))

(define (improve guess r)
  (/ (+ (/ r (square guess)) (* 2 guess))
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess r)
  (< (abs (- (cube guess) r)) 0.001))

(define (cube x)
  (* x x x))

(define (abs x)
  (if (< x 0) (- x) x))

;; Need a way to get it started, 1 can be the first guess everytime.
(define (cbrt x)
  (cube-er 1.0 x))