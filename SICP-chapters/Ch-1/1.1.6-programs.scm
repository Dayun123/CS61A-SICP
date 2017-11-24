;; abs value procedure demonstrating case analysis with cond
(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

;; abs value procedure using else clause
(define (abs-else x)
  (cond ((< x 0) (- x))
        (else x)))

;; abs value procedure using if
(define (abs-if x)
  (if (< x 0)
      (- x)
      x))

;; definition of >= operator, x is greater than y or x is equal to y
(define (>= x y)
  (or (> x y) (= x y)))

;; another definition of >= operator, x is not less than y
(define (>= x y)
  (not (< x y)))

;; I want to see what an undefined example looks like with cond
;; If I pass x as 0, it should return undefined
(define (undef x)
  (cond ((> x 0) (- x ))
        ((< x 0) x)))

(undef 0) ;; returns (Unspecified return value) in MIT Scheme