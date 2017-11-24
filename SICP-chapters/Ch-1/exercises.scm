;; Exercise 1.1 : What is the result printed by the interpreter in each
;; expression? Assume the sequence is to be evaluated in the order in which
;; it is presented.

10
;Value: 10
(+ 5 3 4)
;Value: 12
(- 9 1)
;Value: 8
(/ 6 2)
;Value: 3
(+ (* 2 4) (- 4 6))
;Value: 6
(define a 3)
;Value: a
(define b (+ a 1))
;Value: b
(+ a b (* a b))
;Value: 19
(= a b)
;Value: #f
(if (and (> b a) (< b (* a b)))
  b
  a)
;Value: 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16
(+ 2 (if (> b a)b a))
;Value: 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1))
;Value: 16

;; Exercise 1.2 : Translate the following expression into prefix form
;; 5 + 4 + (2 - (3 - ( 6 + 4/5))) / 3(6 - 2)(2 - 7)
(/ (+ 5 4 (- 2 ( - 3 ( + 6 4/5 ))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3 : Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.

;; Strategy, will need three procedures, the one asked for by the exercise,
;; which I'll call sum-square-largest, a procedure to square the
;; numbers, square, and a procedure to sum the squares, sum-of-squares.

(define (sum-square-largest x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

;; Use helper method square and sum the results
(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Easiest, just returns the argument multiplied by itself
(define (square x)
  (* x x))


;; Exercise 1.4 : Our model of evaluation allows from combinations whose
;; operators are compound expressions. Describe the behavior of the
;; following procedure.

;; This operation uses an if special form as the operator. The if special
;; form will either resolve to a + or -. If b is a negative number, the
;; operation will be a - b, but since b is negative this is the same as
;; addition. If b is positive or 0, the operation will be a + b. Either way,
;; the absolute value of b is always added to a.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5 : What behavior will the following produce in
;; applicative-order evaluation vs. normal-order evaluation when the
;; 'test' procedure is run with 0 and (p) as arguments?

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

;; In normal-order evaluation, the call to 'test' will be replaced with
;; the body of test, and the parameters will become the arguments. Since
;; x = 0, the value of the expression is 0, and the procedure 'p' is never
;; run.
;; In applicative-order evaluation, the call to 'test' will result in the
;; parameter (p) being evaluated, which will lead to an infinite loop since
;; 'p' merely calls itself over and over again. The body of 'test' will never
;; get run.
(test 0 (p))


;; Exercise 1.6 : Here is a version of if defined in terms of cond, what will
;; happen if this 'new-if' procedure is used in the square root program?
;; Answer: The square root program will work just as it did with a regular
;; if, since 'new-if' conforms to the same rules as the built-in if.
;; Well, that didn't work....After running the program and getting a
;; 'maximum recursion dept exeeded warning', I realized that the sqrt
;; procedure won't work with 'new-if' because 'new-if' is a procedure, not
;; a special form like 'if', so the arguments to 'new-if' will be evaluated
;; before the body of 'new-if' is run. If we plug 'new-if' into the sqrt
;; routine, we see that the recursive call to 'sqrt-iter' is the 2nd
;; argument to 'new-if', and therefore the interpreter attempts to
;; retrieve a value from this argument by running the 'sqrt-iter' procedure,
;; but this in turn means that we get an infinite loop! If the interpreter
;; used normal-order-evaluation, new-if would work, but since it uses
;; applicative-order evaluation, it doesn't work as a replacement for
;; 'if' in this procedure.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Exercise 1.7 : good-enough? isn't very effective for very small numbers or
;; very large numbers. Explain, then re-implement good-enough? to watch
;; how 'guess' changes from one iteration to the next and stop when the
;; change is a very small fraction of the guess. Does this new procedure
;; work better for very small/large numbers?

;; Explanation: For very small numbers, good-enough? is not ideal because
;; we are only looking for differences of 0.001. If the radicand is
;; smaller than 0.001, then our guesses will stop as soon as they hit
;; this level of precision, but we could still be way off from the actual
;; number.

;; Ex: These aren't even close! Small numbers...
(sqrt 1.134e-20)
;Value: .03125
(square 0.03125)
;Value: .0009765625

;; large numbers closer
(sqrt 234859683490)
;Value: 484623.23870198475
(square 484623.23870198475)
;Value: 234859683490.0009

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

;; Testing with some numbers, much better precision!
(sqrt 1.134e-20)
;Value: 1.064894360957931e-10
(square 1.064894360957931e-10)
;Value: 1.1340000000000002e-20

(sqrt 234859683490)
;Value: 484623.2387019838
(square 484623.2387019838)
;Value: 234859683490. ;; this had 0.0009 in the original implementation

;; Exercise 1.8 : Implement Newton's cube root procedure

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
