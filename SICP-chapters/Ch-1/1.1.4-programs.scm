;; compound procedure 'square'
(define (square x) (* x x))

;; passing a number to squre
(square 4) ;; 16
;; passing an expression to square
(square (2 + 3)) ;; 25
;; passing a call to the square procedure to square
(square (square 3)) ;; 81

;; using square as a building block for other procedures
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 5) ;; 34

;; using sum-of-squares as a building block for other procedures
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 4) ;; 89
