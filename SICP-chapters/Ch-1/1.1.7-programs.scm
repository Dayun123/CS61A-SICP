;; Square roots by newtons method, take a guess and the radicand and keep
;; improving upon that guess until it is within an acceptable tolerance
;; of the root.
(define (sqrt-iter guess r)
  (if (good-enough? guess r)
    guess
    (sqrt-iter (improve guess r) r)))

(define (improve guess r)
  (average guess (/ r guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess r)
  (< (abs (- (square guess) r)) 0.001))

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

;; Need a way to get it started, 1 can be the first guess everytime.
(define (sqrt x)
  (sqrt-iter 1.0 x))