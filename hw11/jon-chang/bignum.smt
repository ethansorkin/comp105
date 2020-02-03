;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Exercise 42

(class Natural Magnitude
    []

    (class-method base () 32768) 
    (class-method new: (i) 
        (if (>= i 0) 
            {(first:rest: Natural (div: i (base Natural)) 
                                  (mod: i (base Natural)))}
            {(error: self 'negative)})) ;; throw exception

    ;; private methods          
    (class-method first:rest: (m d) (if (and: (= m 0) {(= d 0)})
                                       {(new NatZero)}
                                       {(nat:dig: NatNonzero m d)}))

    (method modBase () (subclassResponsibility self))
    (method divBase () (subclassResponsibility self))
    (method timesBase () (subclassResponsibility self))
    (method compare:withLt:withEq:withGt: (n l e g) (subclassResponsibility 
                                                                          self))
    (method plus:carry: (n c) (subclassResponsibility self))
    (method minus:borrow: (n c) (subclassResponsibility self))


    ;; public methods
    (method = (n) (compare:withLt:withEq:withGt: self n {false} {true} {false}))
    (method < (n) (compare:withLt:withEq:withGt: self n {true} {false} {false}))
    (method + (n) (plus:carry: self n 0))
    (method - (n) (minus:borrow: self n 0))
    (method * (n) (subclassResponsibility self))
    (method sdiv: (i) (sdivmod:with: self i [block (x y) x]))
    (method smod: (i) (sdivmod:with: self i [block (x y) y]))
    (method sdivmod:with: (si b) (subclassResponsibility self))
    (method isZero () (subclassResponsibility self))

    (method subtract:withDifference:ifNegative: (n diff neg) 
        (if (<= n self) {(value diff (- self n))} {(value neg)}))

    (method decimal () (digList: self (new List)))
                    
    (method digList: (ds)   ;;private
        (if (isZero self)
          {(if (isEmpty ds) {(addFirst: ds 0)} {ds})} 
          {(digList: (sdiv: self 10) (addFirst: ds (smod: self 10)))}))


    (method print ()
     (do: (decimal self) [block (digit) (print digit)]))
)


(class NatZero Natural
    []
    ;; Abstraction Function
    ;; A (self) = 0   ;; the subclass NatZero is only a representation for the
                      ;; integer zero so any instance of itself is 0

    ;; Any instance of the NatZero class is a valid input
    (method invariant () true)  ; private


    (class-method new () (new super))

    (method modBase () 0)
    (method divBase () self)
    (method timesBase () self)

    (method compare:withLt:withEq:withGt: (n l e g) (if (isZero n) 
                                                      {(value e)} {(value l)}))

    (method plus:carry: (n c) 
      (if (= c 0) 
        {n}
        {(if (isZero n)
          {(new: Natural c)}
          {(initNat:dig: n 
            (plus:carry: self (divBase n) (div: (+ (modBase n) 1)
                                                             (base Natural)))  
            (mod: (+ (modBase n) 1) (base Natural)))})}))

    (method minus:borrow: (n c)
      (if (and: (isZero n) {(= c 0)}) {self} {(error: self 'negative)}))

    (method * (n) self)

    (method sdivmod:with: (i b) (value b (new NatZero) 0))

    (method isZero () true)
)

(class NatNonzero Natural
    [nat dig]

    ;; Abstraction Function
    ;;   Numbers are built up recursively with the next significant digit
    ;;   built using another instance of a Natural integer.  For a singular
    ;;   digit, the natural is a NatZero instance.
    ;; A (nat dig) = (nat * base) + dig  


    ;; The invariant for a NatNonzero is that neither m or d can be less than 0
    ;;   and the digit 9 must be less than or equal to 9.  Also, m and d both
    ;;   can't be zero, or it would be an instance of NatZero.  
    (method invariant:: (m d) (and: (not (and: (= m 0) {(= d 0)})) ;; private 
                                    {(and: (>= m 0) 
                                           {(and: (>= d 0) {(<= d 9)})})}))

    (class-method nat:dig: (m d) (if (= m 0)
                                    {(initNat:dig: (new self) (new NatZero) d)}
                                    {(initNat:dig: (new self) 
                                      (new: Natural m) d)}))

    (method initNat:dig: (n d) (if (and: (isZero n) {(= d 0)})
                                 {(new NatZero)}
                                 {(set nat n) (set dig d) self}))

    (method nat () nat)  ; private
    (method dig () dig)  ; private

    (method modBase () dig)
    (method divBase () nat)

    (method timesBase () (initNat:dig: (new: Natural 1) self 0))

    (method compare:withLt:withEq:withGt: (n lb eb gb)
        (if (isZero n)
          gb
          {(compare:withLt:withEq:withGt: nat (divBase n) lb 
            {(if (= dig (modBase n)) 
              eb
              {(if (< dig (modBase n)) 
                 lb 
                 gb)})} 
            gb)}))

    (method plus:carry: (n c) 
      (if (isZero n) 
          {(plus:carry: n self c)}
          {(initNat:dig: self (plus:carry: nat (divBase n) 
                                (div: (+ (+ dig (modBase n)) c) (base Natural)))
            (mod: (+ (+ dig (modBase n)) c) (base Natural)))}))

    (method borrowFrom: (b) 
        (if (= b 0)
          {self}
          {(if (= dig 0)
            {(initNat:dig: self (borrowFrom: nat 1) 9)}
            {(initNat:dig: self nat (- dig 1))})}))

    (method minus:borrow: (n b) 
      (if (isZero n)
          {(borrowFrom: self b)}
          {(initNat:dig: self (minus:borrow: nat (divBase n) 
                                (if (< (- (- dig (modBase n)) b) 0) {1} {0}))
            (mod: (- (- dig (modBase n)) b) (base Natural)))}))

    (method * (n)
        (if (isZero n)
            {n}
            {(+ (new: Natural (* dig (modBase n))) 
                (+ (timesBase (+ (* nat (new: Natural (modBase n))) 
                                 (* (divBase n) (new: Natural dig)))) 
                    (timesBase (timesBase (* nat (divBase n))))))}))


    (method sdivmod:with: (d b) [locals twodigits q']
        (set twodigits (+ (* (smod: nat d) (base Natural)) dig))
        (set q' (sdiv: nat d))
        (if (< d 0)
          {(error: self 'badDivisor)}
          {(value b
            (initNat:dig: (new: NatNonzero 1) q' (div: twodigits d))
            (mod: twodigits d))}))

    (method isZero () false)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Exercise 43

(class LargeInteger Integer
  [magnitude]

  (class-method withMagnitude: (aNatural) (if (isZero aNatural)
                                            {(new: LargeInteger 0)}
                                            {(magnitude: (new self) aNatural)}))

  (method magnitude () magnitude)

  (method magnitude: (aNatural) (set magnitude aNatural) self)

  (class-method new: (i)
     (if (negative i) 
        {(magnitude: (new LargeNegativeInteger) (new: Natural (negated i)))}
        {(magnitude: (new LargePositiveInteger) (new: Natural i))}))

  (method asLargeInteger () self)
  (method isZero () (isZero magnitude))
  (method = (i) (isZero   (- self i)))
  (method < (i) (negative (- self i)))

  (method smallIntegerGreaterThan: (i)
    (> self (asLargeInteger i)))
)


(class LargePositiveInteger LargeInteger
  []

  (class-method new () (new super))
  (method print () (print magnitude))

  (method negative () false)
  (method nonnegative () true)
  (method strictlyPositive () (not (isZero self)))
  (method negated () (withMagnitude: LargeNegativeInteger magnitude))

  (method lessThanSmallInteger: (i) (< (asLargeInteger i) self))

  (method + (i) (addLargePositiveIntegerTo: i self))

  (method addLargePositiveIntegerTo: (i)
    (withMagnitude: LargePositiveInteger (+ magnitude (magnitude i))))

  (method addLargeNegativeIntegerTo: (i)
    (if (>= magnitude (magnitude i))
      {(withMagnitude: LargePositiveInteger (- magnitude (magnitude i)))}
      {(withMagnitude: LargeNegativeInteger (- (magnitude i) magnitude))}))

  (method addSmallIntegerTo: (i) (+ self (asLargeInteger i)))

  (method * (i) (multiplyByLargePositiveInteger: i self))

  (method multiplyByLargePositiveInteger: (i)
    (withMagnitude: LargePositiveInteger (* magnitude (magnitude i))))

  (method multiplyByLargeNegativeInteger: (i)
    (withMagnitude: LargeNegativeInteger (* magnitude (magnitude i))))

  (method multiplyBySmallInteger: (i) (* self (asLargeInteger i)))

  (method div: (i) (error: self 'longDivision))

  (method sdiv: (i) 
    (if (negative i)
      {(if (= (smod: (magnitude self) (negated i)) 0) 
          {(withMagnitude: LargeNegativeInteger (sdiv: (magnitude self) 
                                                       (negated i)))}
          {(withMagnitude: LargeNegativeInteger 
              (+ (sdiv: (magnitude self) (negated i)) (new: Natural 1)))})}
      {(withMagnitude: LargePositiveInteger (sdiv: (magnitude self) i))}))

  (method smod: (i)
    (if (negative i)
      {(+ (smod: (magnitude self) (negated i)) i)}
      {(smod: (magnitude self) i)}))

)


(class LargeNegativeInteger LargeInteger
  []

  (class-method new () (new super))

  (method print () (print '-) (print magnitude))
  (method negative () true)
  (method nonnegative () false)
  (method strictlyPositive () false)
  (method negated () (withMagnitude: LargePositiveInteger magnitude))

  (method lessThanSmallInteger: (i) (< (asLargeInteger i) self))

  (method + (i) (addLargeNegativeIntegerTo: i self))

  (method addLargePositiveIntegerTo: (i)
    (if (<= magnitude (magnitude i))
      {(withMagnitude: LargePositiveInteger (- (magnitude i) magnitude))}
      {(withMagnitude: LargeNegativeInteger (- magnitude (magnitude i)))}))

  (method addLargeNegativeIntegerTo: (i) 
    (withMagnitude: LargeNegativeInteger (+ magnitude (magnitude i))))

  (method addSmallIntegerTo: (i) (+ self (asLargeInteger i)))

  (method * (i) (multiplyByLargeNegativeInteger: i self))

  (method multiplyByLargePositiveInteger: (i)
    (withMagnitude: LargeNegativeInteger (* magnitude (magnitude i))))

  (method multiplyByLargeNegativeInteger: (i)
    (withMagnitude: LargePositiveInteger (* magnitude (magnitude i))))

  (method multiplyBySmallInteger: (i) (* self (asLargeInteger i)))

  (method div: (i) (error: self 'longDivision))

  (method sdiv: (i) 
    (if (negative i)
      {(withMagnitude: LargePositiveInteger (sdiv: (magnitude self) 
                                                   (negated i)))}
      {(if (= (smod: (magnitude self) i) 0) 
          {(withMagnitude: LargeNegativeInteger (sdiv: (magnitude self) i))}
          {(withMagnitude: LargeNegativeInteger (+ (sdiv: (magnitude self) i) 
                                                         (new: Natural 1)))})}))
  (method smod: (i) 
   (if (negative i)
     {(negated (smod: (magnitude self) (negated i)))}
     {(- i (smod: (magnitude self) i))}))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Exercise 44


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


