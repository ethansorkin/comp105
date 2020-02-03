;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 30

;; a) The function vector-length finds the hypotenuse of the right triangle
;; with side lengths x and y. Literally, it calculates the square root of 
;; (x^2 + y^2).

;; b) It works because the definitions of '+' and '*' are swapped in the let
;; binding. Even though it looks bizzare, you simply interchange the  
;; conventional meanings of '+'' and '*' when reading the function.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2

;; (a)
;; (count a xs) returns the number of top-level elements in xs that are equal 
;; to x. x must be an atom that is not the empty list and xs is in LIST(SEXPo).

;; laws:
;;   (count a '()) == 0
;;   (count a (cons y ys)) == (+ (count a ys) (if (= a y) 1 0)))


(define count (a xs)
    (if (null? xs)
        0
        (+ (count a (cdr xs)) (if (= a (car xs)) 1 0))))

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (count 4 '()) 0)  ;;checks base case where xs = '()
        (check-expect (count 4 '(4)) 1)   ;; checks a singleton on itself
        (check-expect (count 'x '(1 2 x x)) 2) ;; checks recursive case
        (check-expect (count 'x '(x '(x y) 4)) 1) ;; checks that only top-level
                                                ;; elements are counted
        (check-expect (count 'x '(1 2 3)) 0) ;; checks case where a is not found
                                            ;; in a non-empty xs


;; (b)
;; (countall x xs) returns the number of times x occurs anywhere in xs, not only
;; on the top level. x must be an atom that is not the empty list and xs is 
;; in LIST(SEXPo).

;; laws:
;;   (countall x '()) == 0
;;   (countall x (cons y ys)) == (+ (countall x ys) (if (atom? y) 
;;                                                      (if (= x y) 1 0) 
;;                                                      (countall x y)))

(define countall (x xs)
    (if (null? xs)
        0
        (+ (countall x (cdr xs)) (if (atom? (car xs))
                                    (if (= x (car xs)) 1 0)
                                    (countall x (car xs))))))

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (countall 4 '()) 0)  ;;checks base case where xs = '()
        (check-expect (countall 4 '(4)) 1)   ;; checks a singleton on itself
        (check-expect (countall 'x '(1 2 x x)) 2) ;; checks recursive case
        (check-expect (countall 'x '(x '(x y) 4)) 2) ;; checks that all elements
                                                    ;; anywhere are counted
        (check-expect (countall 'x '(x '(x '(x 1) y))) 3) ;; checks nested lists
        (check-expect (countall 'x '(1 2 3)) 0) ;; checks case where a is not 
                                            ;; found in a non-empty xs




;; (e)
;; (sublist-check xs ys) returns #t if xs is the opening subsequence of ys.
;; That is, it returns #t if and only if there exists a list, back, such that
;; ys = (append xs back). This function is a helper function to contig-sublist
;; and is only called when two elements in the original lists are equivalent.
;; xs and ys are both in LIST(ATOM). (Helper function)

;; laws:
;;   (sublist-check '() '()) == #t
;;   (sublist-check '() (cons a as)) == #t
;;   (sublist-check (cons a as) '()) == #f
;;   (sublist-check (cons z zs) (cons a as)) == (if (= z a) 
;;                                                  (sublist-check zs as)
;;                                                   #f )
(define sublist-check (xs ys)
    (if (and (null? ys) (not (null? xs)))
        #f
        (if (null? xs)
            #t
            (if (= (car xs) (car ys))
                (sublist-check (cdr xs) (cdr ys))
                #f ))))

        (check-assert (sublist-check '() '())) ;; checks case of 2 empty lists

        (check-assert (sublist-check '() '(1 2 3))) ;; checks when xs is empty
                                                    ;; and ys is nonempty

        (check-assert (sublist-check '(1 2 3) '(1 2 3))) ;; checks case when
                                                         ;; xs == ys
        ;; checks case when xs is a sublist of ys.
        (check-assert (sublist-check '(a b c) '(a b c 3 4)))

        ;; checks case when xs is a sublist, but not at front
        (check-assert (not (sublist-check '(a b c) '(1 2 a b c))))

        ;; checks case when xs is nonempty, and ys is empty
        (check-assert (not (sublist-check '(1 2 3) '())))

        ;; checks only top-level elements
        (check-assert (not (sublist-check '(1 2 3) '(1 2 '(1 2 3)))))

        ;; checks case where xs is a sublist of ys, but is not continuous 
        ;; from the front
        (check-assert (not (sublist-check '(1 2 3) '(1 1 2 2 3 3))))

        ;; checks case where xs is not a sublit of ys
        (check-assert (not (sublist-check '(1 2 3) '(a b c))))


;; (contig-sublist? xs ys) returns #t if xs is a contiguous subsequence of ys,
;; returns #f otherwise. xs and ys are both in LIST(ATOM). In other words,
;; (contig-sublist? xs ys) returns #t if and only if there exists two other
;; lists, front and back, such that ys = (append (append front xs) back).

;; laws:
;;   (contig-sublist? '() '()) == #t
;;   (contig-sublist? '() (cons a as)) == #t
;;   (contig-sublist? (cons a as) '()) == #f
;;   (contig-sublist? (cons z zs) (cons a as)) == (if (= z a) 
;;                                                  (if (sublist-check zs as)
;;                                                      #t
;;                                                      (contig-sublist? xs as) 
;;                                                  (contig-sublist? xs as))
(define contig-sublist? (xs ys)
    (if (and (null? ys) (not (null? xs)))
        #f
        (if (null? xs)
            #t
            (if (= (car xs) (car ys))
                (if (sublist-check (cdr xs) (cdr ys)) 
                    #t 
                    (contig-sublist? xs (cdr ys)))
                (contig-sublist? xs (cdr ys))))))


        (check-assert (contig-sublist? '() '())) ;; checks case of 2 empty lists
        (check-assert (contig-sublist? '() '(1 2 3))) ;; checks when xs is empty
                                                      ;; and ys is nonempty
        (check-assert (contig-sublist? '(1 2 3) '(1 2 3))) ;; checks case when
                                                           ;; xs == ys
        
        ;; checks case when 'front' is empty and xs is a contig-sublist.
        (check-assert (contig-sublist? '(a b c) '(a b c 3 4)))

        ;; checks case when 'back' is empty and xs is a contig-sublist
        (check-assert (contig-sublist? '(a b c) '(1 2 a b c)))

        ;; checks when xs is a contig-sublist with nonempty 'front' and 'back'
        (check-assert (contig-sublist? '(a b c) '(1 2 a b c 3 4)))

        ;; checks case when xs is nonempty, and ys is empty
        (check-assert (not (contig-sublist? '(1 2 3) '())))

        ;; checks only top-level elements
        (check-assert (not (contig-sublist? '(1 2 3) '(1 2 '(1 2 3)))))

        ;; checks case where xs is a sublist of ys, but not continuous
        (check-assert (not (contig-sublist? '(1 2 3) '(1 1 2 2 3 3))))

        ;; checks case where xs is not a sublit of ys
        (check-assert (not (contig-sublist? '(1 2 3) '(a b c))))


;; (f)
;; (sublist? xs ys) returns #t if xs is a mathematical subsequence of ys.
;; xs and ys are both in LIST(ATOM). That is, (sublist? xs ys) returns #t if 
;; and only if the list ys contains the elements of xs, in the same order, but 
;; not necessarily contiguous. 

;; laws:
;;   (sublist? '() '()) == #t
;;   (sublist? '() (cons a as)) == #t
;;   (sublist? (cons a as) '()) == #f
;;   (sublist? (cons z zs) (cons a as)) == (if (= z a) 
;;                                                  (sublist? zs as) 
;;                                                  (sublist? (cons z zs) as))

(define sublist? (xs ys)
    (if (and (null? ys) (not (null? xs)))
        #f
        (if (null? xs)
            #t
            (if (= (car xs) (car ys))
                (sublist? (cdr xs) (cdr ys))
                (sublist? xs (cdr ys))))))

        
        (check-assert (sublist? '() '())) ;; checks case of 2 empty lists
        (check-assert (sublist? '() '(1 2 3))) ;; checks when xs is empty
                                                      ;; and ys is nonempty
        ;; checks case when sublist is contiguous
        (check-assert (sublist? '(a b c) '(1 a b c 8 f)))
        ;;checks case when sublist is not contiguous
        (check-assert (sublist? '(a b c) '(1 a 2 b 8 f c)))

        ;; checks case when xs is nonempty, and ys is empty
        (check-assert (not (sublist? '(1 2 3) '())))

        ;; checks only top-level elements
        (check-assert (not (sublist? '(1 2 3) '(1 2 '(1 2 3)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 10


;; (takewhile p? xs) takes a predicate and a list and returns the longest
;; prefix of the list in which every element satisfies the predicate.

;; laws:
;;   (takewhile p? '()) == ()
;;   (takewhile p? (cons y ys)) == (if (p? y)
;;                                     (cons y (takewhile p? ys))
;;                                     '())

(define takewhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
            '())))

               
        (check-expect (takewhile boolean? '()) '()) ;; checks when xs is empty 
        
        ;; checks number? predicate and returns prefix of numbers
        (check-expect (takewhile number? '(1 2 3 a 2 4)) '(1 2 3))

        ;; checks boolean? predicate and returns prefix of bools
        (check-expect (takewhile boolean? '(#t #f 3 a 2 4)) '(#t #f))

        ;;checks when prefix satisfying predicate does not exist
        (check-expect (takewhile atom? '('(1 2 3) #t #f)) '())



;; (dropwhile p? xs) takes a predicate and a list, removes the longest prefix
;; of the list in which every element satisfies the predicate, and returns a
;; list containing the remainder of the elements.

;; laws:
;;   (dropwhile p? '()) == '()
;;   (dropwhile p? (cons y ys) == (if (p? y)
;;                                    (dropwhile p? ys)
;;                                    (cons y ys))))


(define dropwhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
            xs)))

        (check-expect (dropwhile boolean? '()) '()) ;; checks when xs is empty 
        
        ;; checks number? predicate and removes prefix of numbers
        (check-expect (dropwhile number? '(1 2 3 a 2 4)) '(a 2 4))

        ;; checks boolean? predicate and removes prefix of bools
        (check-expect (dropwhile boolean? '(#t #f 3 a 2 4)) '(3 a 2 4))

        ;;checks when prefix satisfying predicate does not exist
        (check-expect (dropwhile atom? '('(1 2 3) #t #f)) '('(1 2 3) #t #f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) takes a natural number and a list of values, and returns the 
;; longest prefix of xs containing at most n elements.

;; laws:
;;   (take 0 '()) == '()
;;   (take 0 (cons y ys)) == '()
;;   (take n '()) == '()
;;   (take n (cons y ys)) == (cons y ys), if n >= # elements in xs
;;   (take n (cons y ys)) == (cons y (take (- n 1) ys)), if n < # elems in xs


(define take (n xs)
    (if (= n 0)
        '()
        (if (null? xs)
            '()
            (cons (car xs) (take (- n 1) (cdr xs))))))

        
        (check-expect (take 0 '()) '())  ;; checks when n == 0 and xs is empty
        (check-expect (take 0 '(a b c 1 2 #t)) '()) ;;checks when n == 0 and
                                                    ;; xs is not empty
        (check-expect (take 5 '()) '()) ;; checks when n > 0 and xs is empty

        ;; checks when n == # elements in xs
        (check-expect (take 3 '(1 #f '(a b))) '(1 #f '(a b)))

        ;; checks when n < # elements in xs
        (check-expect (take 2 '(1 #f '(a b) 3)) '(1 #f))

        ;; checks when n > # elements in xs
        (check-expect (take 5 '(1 #f a)) '(1 #f a))



;; (drop n xs) takes a natural number and a list of values, removes the longest
;; prefix of xs with at most n elements, and returns a list containing the
;; remaining elements.

;; laws:
;;   (drop 0 '()) == '()
;;   (drop 0 (cons y ys)) == (cons y ys)
;;   (drop n '()) == '()
;;   (drop n (cons y ys)) == '(), if n >= # elements in xs
;;   (drop n (cons y ys)) == (drop (- n 1) ys), if n < # elems in xs 


(define drop (n xs)
    (if (= n 0)
        xs
        (if (null? xs)
            '()
            (drop (- n 1) (cdr xs)))))

        (check-expect (drop 0 '()) '())  ;; checks when n == 0 and xs is empty
        (check-expect (drop 0 '(a 1 c)) '(a 1 c)) ;;checks when n == 0 and
                                                    ;; xs is not empty
        (check-expect (drop 5 '()) '()) ;; checks when n > 0 and xs is empty

        ;; checks when n == # elements in xs
        (check-expect (drop 3 '(1 #f '(a b))) '())

        ;; checks when n < # elements in xs
        (check-expect (drop 2 '(1 #f '(a b) 3)) '('(a b) 3))

        ;; checks when n > # elements in xs
        (check-expect (drop 5 '(1 #f a)) '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise C


;; (zip xs ys) takes two lists of values, and converts them to an association 
;; list from the elements of the first to the second list respectively. If the
;; lists have different lengths, the association list returned will only contain
;; pairs up to the last element in the shorter list.

;; laws:
;;   (zip '() '()) == '()
;;   (zip (cons a as) (cons z zs)) == (append (bind (car as) (car zs) '()) 
;;                                                   (zip (cdr as) (cdr zs)))))

(define zip (xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (append (bind (car xs) (car ys) '()) (zip (cdr xs) (cdr ys)))))


        (check-expect (zip '() '()) '()) ;; checks when both lists are empty

        ;;checks when both lists are nonempty
        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))

        ;; checks when lists are different lengths
        (check-expect (zip '(1 2 3 4 5) '(a b c)) '((1 a) (2 b) (3 c)))




;; (keys al) takes an association list and returns a list containing every key
;; in the order which they appear. (Helper function)

;; laws:
;;   (keys '()) == '()
;;   (keys (cons y ys)) == (cons (car y) (keys ys)); where y is a list2
                                                ; and ys is an association list

(define keys (al)
    (if (null? al)
        '()
        (cons (caar al) (keys (cdr al)))))

        
        (check-expect (keys '()) '()) ;; checks case when list is empty
        (check-expect (keys '((1 a) (2 b) (3 c))) '(1 2 3)) ;; checks recursive
                                                            ;; case

;; (keys al) takes an association list and returns a list containing every 
;; attribute in the order which they appear. (Helper function)

;; laws:
;;   (attributes '()) == '()
;;   (attributes (cons y ys)) == (cons (cadr y) (attributes ys)); where y is a 
                                          ; list2 and ys is an association list
(define attributes (al)
    (if (null? al)
        '()
        (cons (cadar al) (attributes (cdr al)))))

        (check-expect (attributes '()) '()) ;; checks case when list is empty
        (check-expect (attributes '((1 a) (2 b) (3 c))) '(a b c)) ;; checks
                                                              ;; recursive case

;; (unzip ps) takes an association list and converts it to a pair of lists,
;; the first containing keys, and the second containing respective attributes.

;; excused from writing algebraic laws for unzip according to HW specs
(define unzip (ps)
    (if (null? ps)
        '(() ())
        (list2 (keys ps) (attributes ps))))

        
        (check-expect (unzip '()) '(() ())) ;;checks case when list is empty

        ;; checks recursive case
        (check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))

        ;;TESTS FOR FURTHER PROPERTIES (from spec)
        (val pairs '((class professor) (11 Laney) (40 Mark) (105 Norman)))
        (check-expect (zip (car (unzip pairs)) (cadr (unzip pairs))) pairs)

        (val xs '(11 40 105)) (val ys '(Laney Mark Norman))
        (check-expect (unzip (zip xs ys)) (list2 xs ys))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise N

;; 1. A LIST1(A) can be defined as and element in the following set:
;; {{LIST(A)} \ '()}. 
;;
;; In other words, the set of all LIST1(A)s is a subset of the set containing 
;; all LIST(A)s, that does not include '().

;; 2. A LIST1(A) can be defined as (cons a as), where a is an A and as is a list
;; of A's.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise D


;; (arg-max f xs) takes a function f that maps a value in set X to a number, and
;; a nonempty list xs of values in set X. It returns the element x in xs for
;; which (f x) has the largest value among all values in xs.

;; laws:
;;   (arg-max f xs) == (find (max* (map f xs)) (zip (map f xs) xs))
;;
;;   Only one algebraic law because the input is restricted to one form of data

(define arg-max (f xs)
        (find (max* (map f xs)) (zip (map f xs) xs)))

        (define double (n) (* 2 n)) ;;returns double the given number, n
        (check-expect (arg-max double '(1 4 2 7 9 3)) 9) ;;tests set of numbers

        (define bool-val (b) (if (= b #t) 1 0)) ;;returns 0 when boolean b == #f
                                                ;;and 1 when b == t
        (check-expect (arg-max bool-val '(#t #f)) #t) ;;tests set of booleans  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E


(record point (x y)) ;; defines point record for future use of the four 
                ;; functions it declares: make-point, point?, point-x, point-y

;; (rightmost-point ps) takes a nonempty list of point records and returns the
;; point with the largest x coordinate. Ties are broken arbitrarily.

;; excused from writing algebraic laws for unzip according to HW specs


(define rightmost-point (ps)
    (find (max* (map point-x ps)) (zip (map point-x ps) ps)))

        ;;checks singleton list
        (check-expect (rightmost-point '((make-point 1 2))) (make-point 1 2)) 

        ;;checks recursive case
        (check-expect (rightmost-point '((make-point 1 2) (make-point 5 6) 
                                            (make-point 3 4))) (make-point 5 6))

        ;;checks case with a tie for the largest x coordinate
        (check-expect (rightmost-point '((make-point 5 6) (make-point 1 2) 
                                         (make-point 5 10) (make-point 3 4))) 
                                                               (make-point 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise F


;; (remove-one-copy x xs) takes an S-expression, x, and a list of S-expressions,
;; xs, which contains one or more copies of x. The function returns a sublist
;; of xs where the first copy of x is removed and all other elements are in the 
;; same order. If xs does not contain x, it is a checked run-time error.

;; laws:
;;   (remove-one-copy x '()) == (error '(x not in xs))
;;   (remove-one-copy x (cons y ys)) == (if (equal? x (car xs))
;;                                          (cdr xs)
;;                                          (cons (car xs) (remove-one-copy x 
;;                                                                  (cdr xs))))


(define remove-one-copy (x xs)
    (if (null? xs)
        (error '(x not in xs))
        (if (equal? x (car xs))
            (cdr xs)
            (cons (car xs) (remove-one-copy x (cdr xs))))))

        ;; checks contract violation with empty list
        (check-error (remove-one-copy 1 '())) 

        ;; checks contract violation when x is not in the nonempty list xs
        (check-error (remove-one-copy 'a '(b e 4 #t)))

        ;; checks case when xs is a singleton list containing x
        (check-expect (remove-one-copy 3 '(3)) '())

        ;;checks recursive case with one copy of x in xs
        (check-expect (remove-one-copy #t '(#f #t 1 2 a)) '(#f 1 2 a))

        ;;checks recursive case with multiple copies of x in xs
        (check-expect (remove-one-copy 2 '(#f 2 1 2 a 2)) '(#f 1 2 a 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise G



;; (permutation? xs ys) takes two lists and returns true if and only if the 
;; lists have the exact same elements and length, regardless of their order.

;; laws:
;;   (permutation? '() '()) == #t
;;   (permutation? '() (cons z zs)) == #f
;;   (permutation? (cons z zs) '()) == #f
;;   (permutation (cons a as) (cons z zs)) ==
;;                         (if (exists? ((curry =) a) (cons z zs))
;;                             (permutation? as (remove-one-copy a (cons z zs)))
;;                             #f )

(define permutation? (xs ys)
    (if (and (null? xs) (null? ys))
        #t
        (if (or (null? xs) (null? ys))
            #f
            (if (exists? ((curry =) (car xs)) ys) ;;checks if (car xs) is in ys
                (permutation? (cdr xs) (remove-one-copy (car xs) ys))
                #f ))))

        (check-assert (permutation? '() '())) ;;checks when both lists are empty

        ;; checks when xs is empty and ys is nonempty
        (check-assert (not (permutation? '() '(1 2 3))))

        ;; checks when xs is nonempty and ys is empty
        (check-assert (not (permutation? '(#t e 2) '())))

        ;; checks passing permutations with the same order
        (check-assert (permutation? '(1 2 3 4) '(1 2 3 4)))

        ;; checks passing permutations with different orders
        (check-assert (permutation? '(8 45 #t p) '(#t p 45 8)))

        ;; checks passing permutation with repeating elements
        (check-assert (permutation? '(a a b b c) '(a b c a b)))

        ;;checks failing permutation
        (check-assert (not (permutation? '(#t 1 2) '(1 2 3))))

        ;;checks failing permutation with repeating elements
        (check-assert (not (permutation? '(1 1 2 3) '(1 1 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise H

;; (length xs) takes a list and returns the number of elements in the list

;; laws:
;;   (length '()) = 0
;;   (length (cons y ys)) == (+ 1 (length ys))

(define length (xs) (if (null? xs) 0 (+ 1 (length (cdr xs)))))

        (check-expect (length '()) 0) ;; checks base case when xs is empty
        (check-expect (length '(1 a #t)) 3) ;;checks recursive case
        (check-expect (length '(#f '() '(a b) 7)) 4) ;;checks that only 
                                              ;; top-level elements are counted

;; (split-list originals) takes a list of values, splits it approximately in 
;; half, and returns a (list2 ys zs) where ys contains the first half of 
;; originals, and zs contains the second half of originals. Therefore 
;; (append ys zs) == originals and the lengths of ys and zs differ by at most 1.

;; Note: The spec states that (append ys zs) only needs to be a permutation of 
;; originals, but my implementation always places the elements in their original
;; order, so I extended the contract to account for this fact.

;; laws:
;;  (split-list xs) == (list2 (take (/ (length xs) 2) xs) 
;;                            (drop (/ (length xs) 2) xs)); where xs is nonempty
;;
;; Only one algebraic law because there is only one case which behaves the same
;; on any appropriate form of data. 

(define split-list (originals)
    (list2 (take (/ (length originals) 2) originals) 
                                     (drop (/ (length originals) 2) originals)))

        (check-expect (split-list '()) '(() ()) ) ;; checks empty list base case

        ;;checks case with even number of elements
        (check-expect (split-list '(1 2 3 a b c)) '((1 2 3) (a b c)))

        ;;checks case with odd number of elements
        (check-expect (split-list '(a #t '() 6 0)) '((a #t) ('() 6 0)))

        ;;checks that (append ys zs) == originals
        (val ns '(1 2 3 4 5))
        (check-expect (append (car (split-list ns)) (cadr (split-list ns))) ns)

        ;;checks that lengths of ys and zs are equal for even number of elements
        (val es '(1 2 3 4 5 6))
        (check-expect (length (car (split-list es))) 
                                                (length (cadr (split-list es))))

        ;;checks that lengths of ys and zs differ by 1 for odd number of elems
        (check-expect (+ 1 (length (car (split-list xs))))
                                                (length (cadr (split-list xs))))



;; (merge xs ys) takes two lists of numbers in ascending order, and returns a
;; single list, sorted in increasing order, containing exactly the same elements
;; as the two argument lists combined.

;; laws:
;;   (merge '() '()) == '()
;;   (merge '() (cons a as)) == (cons a as)
;;   (merge (cons a as) '()) == (cons a as)
;;   (merge (cons a as) (cons z zs)) == (if (< (car xs) (car ys))
;;                                          (cons (car xs) (merge (cdr xs) ys))
;;                                          (cons (car ys) (merge xs (cdr ys))))


(define merge (xs ys)
    (if (null? xs)
        ys
        (if (null? ys)
            xs
            (if (< (car xs) (car ys))
                (cons (car xs) (merge (cdr xs) ys))
                (cons (car ys) (merge xs (cdr ys)))))))


        (check-expect (merge '() '()) '()) ;;checks when both lists are empty

        ;;checks when xs is empty and ys is nonempty
        (check-expect (merge '(1 2 3) '()) '(1 2 3))

        ;;checks when xs is nonempty and ys is empty
        (check-expect (merge '() '(3 7 10)) '(3 7 10))

        ;; checks when both lists are nonempty
        (check-expect (merge '(7 8 15) '(3 7 10)) '(3 7 7 8 10 15))

        ;; checks lists of different lengths
        (check-expect (merge '(1 2 8 100 101) '(1 99)) '(1 1 2 8 99 100 101))
