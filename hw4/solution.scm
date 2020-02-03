;;;;;;;;;;;;;;;;;;; COMP 105 HOF ASSIGNMENT ;;;;;;;;;;;;;;;              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 14

;; (b)
;; (max* xs) takes a non-empty list of integers and returns the largest element.
;; Passing in an empty list results in a run-time error.

;; No algebraic laws are needed

(define max* (xs)
    (foldl max (car xs) (cdr xs)))

        (check-expect (max* '(1)) 1) ;; checks singleton list
        (check-expect (max* '(1 2 3 4 5)) 5) ;; checks list of ascending order
        (check-expect (max* '(5 4 3 2 1)) 5) ;; checks list of descending order
        (check-expect (max* '(5 7 1 4 9 3 2)) 9) ;; checks list of random order
        (check-error (max* '())) ;;checks contract violation


;; (c)
;; (gcd* xs) takes a non-empty list of integers, and returns the greatest common
;; divisor of the elements. Passing an empty list results in a run-time error.

;; No algebraic laws needed

(define gcd* (xs)
    (foldl gcd (car xs) (cdr xs)))

        (check-expect (gcd* '(1)) 1) ;; checks singleton list
        (check-expect (gcd* '(4 12 48 36 100)) 4) ;;checks list w/ common denom
        (check-expect (gcd* '(2 7 100 12)) 1) ;;checks list with no common denom
        (check-error (gcd* '())) ;;checks contract violation


;; (d)
;; (lcm* xs) takes a non-empty list of integers, and returns the least common
;; multiple of the elements. Passing an empty list results in a run-time error.

;; No algebraic laws needed

(define lcm* (xs)
    (foldl lcm (car xs) (cdr xs)))

        (check-expect (lcm* '(1)) 1) ;; checks singleton list
        (check-expect (lcm* '(1 3 9 4)) 36) ;;checks list with multiple elements
        (check-error (lcm* '())) ;;checks contract violation


;; (e)
;; (sum xs) takes a non-empty list of integers and returns the sum of every
;; element. Passing an empty list results in a run-time error.

;; No algebraic laws needed

(define sum (xs)
    (foldl + (car xs) (cdr xs)))

        (check-expect (sum '(1)) 1) ;; checks singleton list
        (check-expect (sum '(1 10 5 -1)) 15) ;;checks list with multiple elems
        (check-error (sum '())) ;;checks contract violation


;; (f)
;; (product xs) takes a non-empty list of integers and returns the product of
;; every element. Passing an empty list results in a run-time error.

;; No algebraic laws needed

(define product (xs)
    (foldl * (car xs) (cdr xs)))

        (check-expect (product '(1)) 1) ;; checks singleton list
        (check-expect (product '(2 3 -1 -4)) 24) ;;checks list of multiple elems
        (check-error (product '())) ;;checks contract violation


;; (h)
;; (append xs ys) takes two lists and returns a list that represents xs followed
;; by ys.

;; No algebraic laws needed

(define append (xs ys)
    (foldr cons ys xs))

        (check-expect (append '() '()) '()) ;; checks with two empty lists

        ;; checks when xs is non-empty and ys is empty
        (check-expect (append '(1 2 3) '()) '(1 2 3))

        ;;c hecks when ys is non-empty and xs is empty
        (check-expect (append '() '(a b c)) '(a b c))

        ;; checks two non-empty lists
        (check-expect (append '(1 b #f) '(#t '(a 3))) '(1 b #f #t '(a 3)))



;; (j)
;; (reverse xs) takes a list and returns the same list in reverse order.

;; No algebraic laws needed

(define reverse (xs)
    (foldl cons '() xs))

        (check-expect (reverse '()) '()) ;;checks empty list
        (check-expect (reverse '(1)) '(1)) ;; checks singleton list
        (check-expect (reverse '(3 #t c)) '(c #t 3)) ;;checks longer list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 15

;; (length xs) takes a list and returns the number of elements in that list

;; No algebraic laws needed

(define length (xs)
    (foldr + 0 (map (lambda (x) 1) xs)))

        (check-expect (length '()) 0) ;;checks empty list
        (check-expect (length '(8)) 1) ;; checks singleton list
        (check-expect (length '(3 #t c '())) 4) ;;checks longer list


;; (map f xs) takes a function and a list and returns a list containing the
;; results of passing every element to f.

;; No algebraic laws needed

(define map (f xs)
    (foldr (lambda (y ys) (cons (f y) ys)) '() xs))

        (check-expect (map number? '()) '()) ;;checks empty list

        ;;checks when f is a predicate, returning booleans
        (check-expect (map number? '(1 s '() 3)) '(#t #f #f #t))

        ;;checks when f returns values when given integers
        (check-expect (map (lambda (y) (* y y)) '(1 2 3 4)) '(1 4 9 16))


;; (filter p? xs) takes a predicate and a list, and returns a new list 
;; containing the elements of xs that satisfy the predicate.

;; No algebraic laws needed

(define filter (p? xs)
    (foldr (lambda (y ys) (if (p? y) (cons y ys) ys)) '() xs))

    (check-expect (filter boolean? '()) '()) ;; checks empty list

    ;; checks non-empty list where all values pass predicate
    (check-expect (filter atom? '(() e 3 #f)) '(() e 3 #f))

    ;; checks nonempty list where some elements pass predicate
    (check-expect (filter boolean? '(1 2 #f '() #t '(a b))) '(#f #t))

    ;; checks nonempty list where no elements pass predicate
    (check-expect (filter number? '(#t a b ())) '())


;; (exists? p? xs) takes a predicate and a list. It returns #t if any element
;; of the list satisfies the predicate, and returns #f otherwise.

;; No algebraic laws needed

(define exists? (p? xs)
    (foldl (lambda (x y) (if y y x)) #f (map p? xs)))

        (check-expect (exists? number? '()) #f) ;; checks empty list
        (check-expect (all? number? '(5)) #t) ;; checks passing singleton
        (check-expect (all? boolean? '(5)) #f) ;; checks failing singleton

        ;;checks list where all elements satisfy predicate
        (check-expect (exists? boolean? '(#t #f #t)) #t)

        ;;checks list where one element satisfies predicate
        (check-expect (exists? null? '(#t 1 ())) #t)

        ;;checks list where no elements satisfy predicate
        (check-expect (exists? number? '(#t '(1 2) e)) #f)


;; (all? p? xs) takes a predicate and a list. It returns #t if every element
;; of the list satisfies the predicate, and returns #f otherwise.

;; No algebraic laws needed

(define all? (p? xs)
    (foldl (lambda (x y) (if y x y)) #t (map p? xs)))

        (check-expect (all? number? '()) #t) ;; checks empty list
        (check-expect (all? number? '(5)) #t) ;; checks passing singleton
        (check-expect (all? boolean? '(5)) #f) ;; checks failing singleton

        ;;checks list where all elements satisfy predicate
        (check-expect (all? boolean? '(#t #f #t)) #t)

        ;;checks list where only first element fails predicate
        (check-expect (all? null? '(1 () ())) #f)

        ;;checks list where only last element fails predicate
        (check-expect (all? atom? '(#t 3 () '(hello))) #f)

        ;;checks list where no elements satisfy predicate
        (check-expect (all? number? '(#t '(1 2) e)) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 19


;; (a)
;; 'evens' is a set containing all the even integers
(val evens (lambda (x) (if (number? x) (= (mod x 2) 0) #f)))

;; (b)
;; 'two-digits' is a set which contains all two-digit positive numbers
(val two-digits (lambda (x) (if (number? x) (and (>= x 10) (<= x 99)) #f)))

;; (c)
;; PROPERTIES:
;;    (member? x (add-element x s)) == #t
;;    (member? y (add-element x s)) == (s y), where (not (equal? y x))
;;    (member? x (union s1 s2)) == (or (s1 y) (s2 y))
;;    (member? x (inter s1 s2)) == (and (s1 y) (s2 y))
;;    (member? x (diff s1 s2)) == (and (s1 y) (not (s2 y)))

(define add-element (x s)
    (lambda (y) (or (s y) (equal? x y))))

(define union (s1 s2)
    (lambda (y) (or (s1 y) (s2 y))))

(define inter (s1 s2)
    (lambda (y) (and (s1 y) (s2 y))))

(define diff (s1 s2)
    (lambda (y) (and (s1 y) (not (s2 y)))))

        ;; definition of member? from p.216 of book, used to test whether an 
        ;; element x is a member of the given set s.
        (define member? (x s) (s x))

        ;;Unit tests for parts a, b, c
        ;;(a)
        (check-assert (member? 4 evens))  ;;checks evens definition, true case
        (check-assert (not (member? 5 evens)))  ;;checks evens, false case

        ;;(b)
        (check-assert (member? 63 two-digits))  ;;checks two-digits, true case
        (check-assert (not (member? 100 two-digits)))  ;;checks false two-digits
        (check-assert (not (member? 9 two-digits)))  ;;checks false two-digits

        ;;(c)
        ;; checks for add-element properties
        (check-assert (member? 3 (add-element 3 evens))) ;;checks what was added
        (check-assert (not (member? 5 (add-element 3 evens))))
                                            ;;checks something that wasn't added
        ;; checks for union properties
        (check-assert (member? 44 (union evens two-digits))) ;; 44 is in both
        (check-assert (member? 4 (union evens two-digits))) ;; 4 is in evens
        (check-assert (member? 11 (union evens two-digits))) ;; 11 in two-digits
        (check-assert (not (member? 7 (union evens two-digits)))) 
                                                              ;; 7 is in neither

        ;; checks for inter properties
        (check-assert (member? 44 (inter evens two-digits))) ;; 44 is in both
        (check-assert (not (member? 4 (inter evens two-digits)))) ;; 4 in evens
        (check-assert (not (member? 11 (inter evens two-digits)))) 
                                                         ;;11 only in two-digits
        (check-assert (not (member? 7 (inter evens two-digits)))) 
                                                              ;; 7 is in neither

        ;; checks for diff properties
        (check-assert (member? 4 (diff evens two-digits))) ;; 4 is only in evens
        (check-assert (not (member? 12 (diff evens two-digits)))) 
                                                      ;;12 is only in two-digits
        (check-assert (not (member? 44 (diff evens two-digits)))) 
                                                              ;; 44 is in both
        (check-assert (not (member? 7 (inter evens two-digits)))) 
                                                              ;; 7 is in neither


;; (d)
(record set-ops (empty member? add-element union inter diff))

(define set-ops-from (eq?)
    (let ([empty (lambda (x) #f)]
          [member? (lambda (x s) (s x))]
          [add-element (lambda (x s) (lambda (y) (or (s y) (eq? x y))))]
          [union (lambda (s1 s2) (lambda (y) (or (s1 y) (s2 y))))]
          [inter (lambda (s1 s2) (lambda (y) (and (s1 y) (s2 y))))]
          [diff (lambda (s1 s2) (lambda (y) (and (s1 y) (not (s2 y)))))])
     (make-set-ops empty member? add-element union inter diff)))

        ;;Recommended unit tests
        (check-assert (procedure? set-ops-from))
        (check-assert (set-ops? (set-ops-from =)))

(val a-set-ops (set-ops-from =))
(val a-emptyset      (set-ops-empty a-set-ops))
(val a-member?      (set-ops-member? a-set-ops))
(val a-add-element  (set-ops-add-element a-set-ops)) 
(val a-union        (set-ops-union a-set-ops))
(val a-inter        (set-ops-inter a-set-ops))
(val a-diff         (set-ops-diff a-set-ops))

        ;;Unit tests
        ;; a-emptylist
        (check-assert (not (a-member? 5  a-emptyset)))

        ;; a-add-element
        (check-assert (a-member? #t (a-add-element #t a-emptyset)))
        (check-assert (not (a-member? #f (a-add-element #t a-emptyset))))

        ;; a-union
        (check-assert (a-member? 4 (a-union evens two-digits)))
        (check-assert (a-member? 11 (a-union evens two-digits)))
        (check-assert (not (a-member? 7 (a-union evens two-digits))))

        ;; a-inter
        (check-assert (a-member? 12 (a-inter evens two-digits)))
        (check-assert (not (a-member? 4 (a-inter evens two-digits))))
        (check-assert (not (a-member? 11 (a-inter evens two-digits))))

        ;; a-diff
        (check-assert (a-member? 4 (a-diff evens two-digits)))
        (check-assert (not (a-member? 77 (a-diff evens two-digits))))
        (check-assert (not (a-member? 48 (a-diff evens two-digits))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise A

(define f-functional (y)
    (letrec ([x e]
             [set-fun (lambda (xp) (if (p? xp y) (set-fun (g xp y)) (h xp y)))])
        (set-fun x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise F

;; (flip f) takes a two argument function and returns a function that expects
;; its arguments in the opposite order.

;; Laws:
;;    (flip f) == (lambda (x y) (f y x))
;;    ((flip f) x y) == (f y x)

(define flip (f)
    (lambda (x y) (f y x)))

        (check-assert (procedure? (flip <)))
        (check-expect ((flip >) 3 4) (< 3 4))
        (check-expect ((flip append) '(4 5 6) '(1 2 3)) (append '(1 2 3) 
                                                                      '(4 5 6)))
        (check-expect ((flip cons) '(#t c '()) 1) (cons 1 '(#t c '())))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise G1

(record edge [from to]) ;; defining an edge record

;; (edge-list g) takes a graph represented as a successors map and returns a
;; list of all the edges in the graph in any order.

;; (find-edges n) takes a node 'n' of a successors map and returns a list of 
;; every possible edge that can be made with that node and its immmediate 
;; successors. (Internal helper)

;; Laws:
;;   (edge-list '()) == '()
;;   (edge-list (bind node successors graph)) ==
;;                            (append (list2 node successors) (edge-list graph))

;; Internal function laws:
;;   (find-edges (list2 node '())) == '()
;;   (find-edges (list2 node successors)) == 
;;                             (cons (make-edge node (car successors)) 
;;                                   (find-edges (list2 node (cdr successors))))

(define edge-list (g)
    (letrec ([find-edges    
                (lambda (n)
                    (if (null? (cadr n))
                        '()
                        (cons (make-edge (car n) (caadr n)) 
                                     (find-edges (list2 (car n) (cdadr n))))))])
     (if (null? g)
        '()
        (append (find-edges (car g)) (edge-list (cdr g))))))


        (val graph '([A (B C)] [B (D)] [C (D)] [D ()])) ;;graph for testing
;;      A --> B
;;      |     |
;;      v     v
;;      C --> D

        (check-expect (edge-list '()) '()) ;;checks empty successors map
        (check-expect (edge-list graph) '((make-edge A B) (make-edge A C)
                                               (make-edge B D) (make-edge C D)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise G2

;; (add-edge e g) takes an edge made with make-edge and a graph represented by
;; a successors map. It returns a new graph that is like the original g, except
;; the given edge e has been added to it.

;; No algebraic laws needed (no recursion)

(define add-edge (e g)
    (let ([update-node  (lambda (n) 
                            (if (equal? (edge-from e) (car n))
                                (list2 (car n) (cons (edge-to e) (cadr n)))
                                n))])
     (if (exists? ((curry equal?) e) (edge-list g))
        g
        (if (null? (find (edge-from e) g))
            (bind (edge-from e) (list1 (edge-to e)) g)
            (map update-node g)))))

        ;;The following two functions are used for testing (from solutions)
        (define remove-one-copy (x xs)
          (if (null? xs)
              (error 'removed-an-absent-item)
              (if (equal? x (car xs))
                  (cdr xs)
                  (cons (car xs) (remove-one-copy x (cdr xs))))))
        (define permutation? (xs ys)
          (if (null? xs)
              (null? ys)
              (let ([z  (car xs)]
                    [zs (cdr xs)]
                    [member? (lambda (a as) (exists? ((curry equal?) a) as))])
                (if (member? z ys)
                    (permutation? zs (remove-one-copy z ys))
                    #f))))

        (val e1 (make-edge 'E 'F)) ;; neither node in graph
        (val e2 (make-edge 'B 'D)) ;; edge exists in graph
        (val e3 (make-edge 'A 'E)) ;; A is a node in graph with successors
        (val e4 (make-edge 'D 'F)) ;; D is a node in graph without successors

        (check-assert (permutation? (cons e1 (edge-list graph))
                                               (edge-list (add-edge e1 graph))))
        (check-assert (permutation? (edge-list graph)
                                               (edge-list (add-edge e2 graph))))
        (check-assert (permutation? (cons e3 (edge-list graph))
                                               (edge-list (add-edge e3 graph))))
        (check-assert (permutation? (cons e4 (edge-list graph))
                                               (edge-list (add-edge e4 graph))))
        
        (check-expect (add-edge e1 '()) '([E (F)]))  ;; empty graph test
        (check-expect (add-edge e1 graph) '([A (B C)] [B (D)] [C (D)] [D ()] 
                                                                      [E (F)]))
        (check-expect (add-edge e2 graph) graph)
        (check-expect (add-edge e3 graph) '([A (E B C)] [B (D)] [C (D)] [D ()]))
        (check-expect (add-edge e4 graph) '([A (B C)] [B (D)] [C (D)] [D (F)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise G3

;; (remove-node node graph) takes a node and a graph represented by a successors
;; map, and returns a new graph that is like the original, but the given node is
;; removed. The resulting graph must not contain the given node as any key or in
;; any list of successors.


(define remove-node (node graph)
    (let ([check-successors  
            (lambda (y) 
                (if (not (exists? ((curry equal?) node) (cadr y)))
                    y
                    (list2 (car y) (filter (lambda (z) (not (equal? node z))) 
                                                                  (cadr y)))))])
     (if (not (exists? ((curry equal?) node) (map car graph)))
        graph
        (map check-successors (filter (lambda (x) (not (equal? node (car x)))) 
                                                                      graph)))))

        ;;graph is defined above in G1 tests
        (check-expect (remove-node 'C graph) '((A (B)) (B (D)) (D ())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise O

;; (ordered-by? f) takes a comparison function that represents a transitive
;; relation, and returns a predicate that tells if a list of values if totally
;; ordered by the relation, f.

;; Laws:
;;   ((ordered-by? f) '()) == #t             ;;empty list
;;   ((ordered-by? f) (cons x '())) == #t    ;;singleton list
;;   ((ordered-by? f) (cons x (cons y ys)) == (and (f x y) (check-order 
;;                                                                 (cons y ys)))


(define ordered-by? (f)
    (letrec ([check-order   (lambda (ys) (if (<= (length ys) 1)
                                             #t
                                             (and (f (car ys) (cadr ys)) 
                                                     (check-order (cdr ys)))))])
     (lambda (xs) (check-order xs))))

        (check-assert (procedure? ordered-by?))
        (check-assert (procedure? (ordered-by? <)))
        (check-error (ordered-by? < '(1 2 3)))
        (check-assert ((ordered-by? and) '()))
        (check-assert ((ordered-by? <=) '(1 2 3)))
        (check-assert (not ((ordered-by? =) '(3 2 1))))
        (check-assert ((ordered-by? or) '(#t #f #t)))
        (check-assert (not ((ordered-by? and) '(#t #f #t))))






