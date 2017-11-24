;; two versions of square program, demonstrating that they can be a
;; black-box as long as we know they square the input
(define (square x)
  (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x)
  (+ x x))

;; Illustrating block structure, the use of internal procedures that
;; will be within sqrt's scope only
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))
    )
  (sqrt-iter 1.0 x));; the actual body of sqrt

;; Demonstrating lexical scoping, the fact that x, while a bound variable
;; in sqrt, can now be a free variable in the internal procedures since the
;; definition of x will come from sqrt anyway, it doesn't need to be passed
;; in as an argument to the internal procedures
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess)))
    )
  (sqrt-iter 1.0));; the actual body of sqrt
