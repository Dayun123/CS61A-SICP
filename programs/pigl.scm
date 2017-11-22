;; The pigl program from Lecture 1

(load "../class-lib-files/simply.scm")

;; Recursively calls itself, moves first letter to end of the word until
;;   the first letter is a vowel, then adds 'ay to the end of the word
(define (pigl wd)
  (if (pl-done? wd)
    (word wd 'ay) ;; tak an 'ay' onto the end of the word
    (pigl (word (bf wd) (first wd))))) ;; put the first letter from the word at the end

;; We are done calling pigl if the first letter of the word is a vowel
(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))