;; Square roots by newtons method, take a guess and the radicand and keep
;; improving upon that guess until it is within an acceptable tolerance
;; of the root.
(define (sqrt-iter guess r last-guess)
  (if (good-enough? guess r last-guess)
    guess
    (sqrt-iter (improve guess r) r guess)))

(define (improve guess r)
  (average guess (/ r guess)))

(define (average x y)
  (/ (+ x y) 2))

;; This version focuses on good-enough? being within a more
;; stringent tolerance, but is looking for that tolerance between the
;; difference of the current guess and last guess, as opposed to the
;; square of the current guess and the radicand, which was the original
;; implementation.
(define (good-enough? guess r last-guess)
  (< (abs (- last-guess guess)) 0.1e-20))

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

;; Need a way to get it started, 1 can be the first guess everytime.
(define (sqrt x)
  (sqrt-iter 1.0 x x))