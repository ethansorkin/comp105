;;;;;;;;;;;;;;;;;;; COMP 105 Continuations ASSIGNMENT ;;;;;;;;;;;;;;;        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise L

;; (list-of? (A? v) takes a predicate that can be applied to any value, A?, and
;; an arbitrary uScheme value, v. It returns #t if and only if v is a list of
;; values, each of which satisfies A?, and returns #f otherwise.

;; Laws:
;;  (list-of? A? x) == #f ; where x is a non-pair, single value (bool, number, 
;;  (list-of? A? '()) == #t                                           or symbol)
;;  (list-of A? (cons y ys)) == (all? A? (cons y ys)) ;;nonempty list

(define list-of? (A? v)
    (if (null? v)
        #t
        (if (pair? v)
            (all? A? v)
            #f)))

        (define value? (_) #t) ;;value predicate used for testing

        (check-assert (not (list-of? value? 1)))
        (check-assert (not (list-of? value? 'e)))
        (check-assert (list-of? value? '()))
        (check-assert (list-of? value? '(1 e #t)))

        (check-assert (not (list-of? number? 1)))
        (check-assert (list-of? number? '()))
        (check-assert (not (list-of? number? '(1 e #t))))
        (check-assert (list-of? number? '(1 2 3)))

        (check-assert (not (list-of? pair? 1)))
        (check-assert (list-of? pair? '()))
        (check-assert (not (list-of? pair? '(()(5)))))
        (check-assert (list-of? pair? '((1 2 (4)) (()) (e #t #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise F

;; (formula? v) takes an arbitrary uScheme value, and returns #t if the value
;; represents a Boolean formula. It returns #f otherwise.

;; Laws:
;;  (formula? s) == #t  ;where s is a symbol
;;  (formula? (make-not f)) == #t ;where f is a formula
;;  (formula? (make-or fs)) == #t ;where fs is list of formulas
;;  (formula? (make-and fs)) == #t ;where fs is list of formulas
;;  (formula? v) == #f ;where v has none of the forms above

;;Record definitions
(record not [arg])
(record or  [args])
(record and [args])


(define formula? (v)
    (if (symbol? v)
        #t
        (if (not? v)
            (formula? (not-arg v))
            (if (or? v)
                (list-of? formula? (or-args v))
                    (if (and? v)
                        (list-of? formula? (and-args v))
                        #f)))))

        (check-assert (formula? 'a))
        (check-assert (formula? (make-not 'f)))
        (check-assert (formula? (make-or '(a b))))
        (check-assert (formula? (make-and '(c (make-not e) #))))
        (check-assert (not (formula? (make-not 1))))
        (check-assert (not (formula? (make-or '(e 1 @)))))
        (check-assert (not (formula? (make-and '((make-not e) ())))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E

;; (eval-formula f env) takes a formula, f, and an environment, env, which is
;; represented by an association list where each key is a symbol and each
;; value is a boolean. If the formula is satisfied in the given environment,
;; this function returns #t, otherwise it returns #f. (eval-formula f env) may 
;; be called only when every variable in formula f is bound in env

;; (check-args x) takes a formula, and returns the value of evaluating the
;; formula in the environment passed in to eval-formula. This helper function
;; recursively calls eval-formula until the base case of a formula (represented
;; by a symbol) is reached. The same restrictions on 'f' in eval-formula apply
;; to 'x' in check-args.

;; Laws:
;;  (eval-formula s env) == (find s env) ;where s is a symbol
;;  (eval-formula (make-not fm) env) == (not (check-args fm))
                                                        ;where fm is a formula
;;  (eval-formula (make-or fs) env) == (exists? check-args (fs))
                                                ;where fs is a list of formulas
;;  (eval-formula (make-and fs) env) == (all? check-args (fs))
                                                ;where fs is a list of formulas

;; Internal function laws:
;;  (check-args s) == (find s env) ;where s is a symbol
;;  (check-args f) == (eval-formula f env) ;where f is any other type of formula


(define eval-formula (f env)
    (letrec ([check-args    (lambda (x) (if (symbol? x) 
                                            (find x env)
                                            (eval-formula x env)))])
     (if (symbol? f)
        (find f env)
        (if (not? f)
            (not (check-args (not-arg f)))
            (if (or? f)
                (exists? check-args (or-args f))
                (all? check-args (and-args f)))))))


        (val env '((a #t) (b #t) (c #f) (d #f))) ;;environment used for testing

        ;; when f is a symbol
        (check-assert (eval-formula 'a env))
        (check-assert (not (eval-formula 'd env)))

        ;; when f is a (make-not f)
        (check-assert (eval-formula (make-not 'd) env))
        (check-assert (not (eval-formula (make-not 'a) env)))
        (check-assert (eval-formula (make-not (make-not 'a)) env))

        ;;when f is a (make-or fs)
        (check-assert (not (eval-formula (make-or '()) env)))
        (check-assert (eval-formula (make-or '(a b c d)) env))
        (check-assert (not (eval-formula (make-or '(c d)) env)))
        (check-assert (eval-formula (make-or '((make-not c) d)) env))

        ;;when f is a (make-and fs)
        (check-assert (eval-formula (make-and '()) env))
        (check-assert (eval-formula (make-and '(a b)) env))
        (check-assert (not (eval-formula (make-and '(a b c)) env)))
        (check-assert (eval-formula (make-and '(a b (make-not (make-or (d c)))))
                                                                           env))
        (check-assert (not (eval-formula (make-and '(a (make-and (b c)))) env)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise S

;; (find-formula-true-asst f fail succ) takes a formula f, a failure 
;; continuation fail, and a success continuation succ. If there exists a
;; satisfying assignment (as an association list) to the formula f, it calls
;; succ. If no such assignment exists, it calls fail.

;; Laws:
;; (formula x bool cur fail succ) == (symbol x bool cur fail succ) 
                                                            ;where x is a symbol

;; (formula (make-not f) bool cur fail succ) == (formula f (not bool) cur fail 
;;                                                                         succ)

;; (formula (make-or fs) #t cur fail succ) == (any fs #t cur fail succ)
;; (formula (make-or fs) #f cur fail succ) == (all fs #f cur fail succ)

;; (formula (make-and fs) #t cur fail succ) == (all fs #t cur fail succ)
;; (formula (make-and fs) #f cur fail succ) == (any fs #f cur fail succ)

;; (all '() bool cur fail succ) == (succ cur fail)
;; (all (cons f fs) bool cur fail succ) == (formula f bool cur fail 
;;                                   (lambda (c r) (all fs bool cur fail succ)))

;; (any '() bool cur fail succ) == (fail)
;; (any (cons f fs) bool cur fail succ) == (formula f bool cur 
;;                                 (lambda () (any fs bool cur fail succ)) succ)

;; (symbol x bool cur fail succ) == (succ (bind x bool cur) fail) 
                                                    ;where x is not bound in cur
;; (symbol x bool cur fail succ) == (succ cur fail) ;where x equals bool in cur
;; (symbol x bool cur fail succ) == (fail) ;where x equals (not bool) in cur


(define find-formula-true-asst (f fail succ)
    (letrec ([formula   
                    (lambda (f bool cur fail succ)
                        (if (symbol? f)
                            (symbol f bool cur fail succ)
                            (if (not? f)
                                (formula (not-arg f) (not bool) cur fail succ)
                                (if bool
                                    (if (or? f)
                                        (any (or-args f) bool cur fail succ)
                                        (all (and-args f) bool cur fail succ))
                                    (if (and? f)
                                        (any (and-args f) bool cur fail succ)
                                        (all (or-args f) bool cur fail succ)
                                                                         )))))]
             [all   (lambda (fs bool cur fail succ)
                        (if (null? fs)
                            (succ cur fail)
                            (formula (car fs) bool cur fail 
                                        (lambda (c r) (all (cdr fs) 
                                                        bool c r succ)))))]
             [any   (lambda (fs bool cur fail succ)
                        (if (null? fs)
                            (fail)
                            (formula (car fs) bool cur 
                                        (lambda () (any (cdr fs)
                                                        bool cur fail succ)) 
                                                                        succ)))]
             [symbol    (lambda (x bool cur fail succ)
                            (if (null? (find x cur))
                                (succ (bind x bool cur) fail)
                                (if (equal? bool (find x cur))
                                    (succ cur fail)
                                    (fail))))])
     (formula f #t '() fail succ)))


        ;;Tests to assure my function has the correct interface
        (check-assert (procedure? find-formula-true-asst))    ; correct name
        (check-error (find-formula-true-asst))                ; not 0 arguments
        (check-error (find-formula-true-asst 'x))             ; not 1 argument
        (check-error (find-formula-true-asst 'x (lambda () 'fail))) ; not 2 args
        (check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                       (lambda (c r) 'succeed) 'z)) ; not 4 args

        ;;Additional checks that require a little bit of a solver
        (check-error (find-formula-true-asst 'x (lambda () 'fail) 
                        (lambda () 'succeed))) ;success cont. expects 2 args
        (check-error (find-formula-true-asst 'x (lambda () 'fail) 
                        (lambda (_) 'succeed))) ; success cont. expects 2 args
        (check-error (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                                      (lambda (_) 'fail) (lambda (_) 'succeed)))
                                       ;failure cont. expects 0 arguments, not 1

        ;;More tests that check the proper continuation with proper arguments
        (check-expect   ; x can be solved
           (find-formula-true-asst 'x (lambda () 'fail)
                                      (lambda (cur resume) 'succeed)) 'succeed)

        (check-expect   ; x is solved by '((x #t))
           (find-formula-true-asst 'x (lambda () 'fail)
                                       (lambda (cur resume) (find 'x cur))) #t)

        (check-expect   ; (make-not 'x) can be solved
           (find-formula-true-asst (make-not 'x) (lambda () 'fail)
                                      (lambda (cur resume) 'succeed)) 'succeed)

        (check-expect   ; (make-not 'x) is solved by '((x #f))
           (find-formula-true-asst (make-not 'x) (lambda () 'fail)
                                       (lambda (cur resume) (find 'x cur))) #f)

        (check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
           (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                       (lambda () 'fail) (lambda (cur resume) 'succeed)) 'fail)

        ;;Other edge cases
        (check-expect (find-formula-true-asst (make-not (make-or '())) 
                          (lambda () 'fail) (lambda (c r) 'succeed)) 'succeed)
        (check-expect (find-formula-true-asst (make-not (make-and '())) 
                          (lambda () 'fail) (lambda (c r) 'succeed)) 'fail)
        (check-expect (find-formula-true-asst 
                       (make-not (make-and '(x (make-not x)))) (lambda () 'fail)
                                              (lambda (c r) 'succeed)) 'succeed)






