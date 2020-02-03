;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Exercise 44

;; All methods for double dispatch needed in the LargeInteger classes are in
;; the file bignum.smt

(use bignum.smt)

(class SmallInteger SmallInteger
  []

  (method asLargeInteger () (new: LargeInteger self))
  (method isZero () (primequal: self 0)) ;; private
  (method primequal: primitive eqObject)

  (method < (i) (lessThanSmallInteger: i self))
  
  (method lessThanSmallInteger: primitive >)

  (method > (i) (< i self))

  (method = (i) (isZero (- self i)))

  (method + (i) (addSmallIntegerTo: i self))

  (method addSmallIntegerTo: (i)
    (value (addSmall:withOverflow: self i {(+ (asLargeInteger self) i)})))

  (method addSmall:withOverflow: primitive add:withOverflow:)

  (method addLargePositiveIntegerTo: (i) (+ (asLargeInteger self) i))

  (method addLargeNegativeIntegerTo: (i) (+ (asLargeInteger self) i))

  (method negated () (value (subSmall:withOverflow: 0 self 
                                {(negated (new: LargeInteger self))})))

  (method - (i) (addSmallIntegerTo: (negated i) self))

  (method * (i) (multiplyBySmallInteger: i self))

  (method multiplyBySmallInteger: (i)
    (value (mulSmall:withOverflow: self i {(* (asLargeInteger self) i)})))

  (method mulSmall:withOverflow: primitive mul:withOverflow:)

  (method multiplyByLargeNegativeInteger: (i) (* (asLargeInteger self)
                                                           i))

  (method multiplyByLargePositiveInteger: (i) (* (asLargeInteger self) i))

  (method subSmall:withOverflow: primitive sub:withOverflow:)
)
