;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Solution to Exercise 39(a)

(class Integer Integer
    ()
    (method num () self)
    (method den () 1)
)

(class SmallInteger SmallInteger
    ()
    (method num () self)
    (method den () 1)
)

  		(check-expect (+ (/ 2 2) 3) (/ 4 1))
  		(check-expect (- (/ 5 2) 1) (/ 3 2))
  		(check-expect (* (/ 1 2) 4) (/ 2 1))
