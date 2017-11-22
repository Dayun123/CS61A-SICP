;; Lab Assignment mentioned in Lecture 1 at 38 min mark
;; Deal with the problem in the original implemenation of plural wherein
;;   words like 'boy' come out 'boies'.

;; Original implementation of plural

(define (plural wd)
  (if (equal? (last wd) 'y)
    (word (bl wd) 'ies)
    (word wd 's')))

;; Implementation that takes into account words ending in 'oy'

(define (plural wd)
  (if (and (equal? (last wd) 'y) (not (equal? (last (bl wd)) 'o)))
    (word (bl wd) 'ies)
    (word wd 's)))