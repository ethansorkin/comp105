;;;;;;;;;;;;;;;;;;; COMP 105 Continuations ASSIGNMENT ;;;;;;;;;;;;;;;     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise T

;;Record definitions
(record not [arg])
(record or  [args])
(record and [args])


; This test case is important because it will always be true, so you do not need
; to bind any variables in the solution. (make-or '()) is the base case of the 
; 'or' helper function, and has no solution because there is no formula in '() 
; that evaluates to #t. Therefore, the complement of (make-or '()) is always #t.
(val f1 (make-not (make-or '())))
(val s1 '())

; This test case is important because it will call the fail continuation after
; checking the first argument of the outer 'make-or' formula, which is a
; lambda we defined that recursively calls the 'or' helper function. After
; reaching the second argument, it detects that if y is bound to #f, then
; (make-not y) evaluated to #t, so the 'make-or' formula is satisfied. We do not
; need to define z.

(val f2 (make-or '((make-and (x (make-not x))) (make-not y) z)))
(val s2 '((y #f)))

; This test case is important because it will call the success continuation
; after checking the first three arguments, extending the environment to bind
; x to #t and y to #f. When it reaches the final argument, it will fail,
; because x is already bound to #t, therefore (make-not x) cannot be satisfied.
(val f3 (make-and '(x (make-and ()) (make-not y) (make-not x))))
(val s3 'no-solution )