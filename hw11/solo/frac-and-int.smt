;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 39(a)

(class SmallInteger SmallInteger
    ()
    (method num () self)
    (method den () 1)
 )


(check-expect (+ (/ 10 3) 2) (/ 16 3))
(check-expect (- (/ 77 3) 16) (/ 29 3))
(check-expect (* (/ 896 49) 49) (/ 896 1))