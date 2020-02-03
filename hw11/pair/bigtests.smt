;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Solution to Exercise T


;; 3 tests will test only class Natural

; Summary: ifNegative block
(check-print 
  (subtract:withDifference:ifNegative: (new: Natural 0) 
    (new: Natural 7) [block (x) (print x)] {(sdiv: (new: Natural 103) 25)})
4)


; Summary: multiple arithmetic tests
(check-print
  (if (= (+ (sdiv: (new: Natural 123) 5) (* (new: Natural 8) (new: Natural 6)))
         (+ (sdiv: (new: Natural 99) 4) (new: Natural (smod: (new: Natural 98)
                                                              50))))
        {(- (new: Natural 1784) (new: Natural 1784))}
        {(print 'FailedEqualityTest)})
0)

; Summary: arbitrarily large numbers
(check-print
  (if (> (sdiv: (* (new: Natural 33333) (new: Natural 1000000)) 32768)
         (new: Natural (smod: (* (new: Natural 33333) (new: Natural 1000000)) 
                               32768)))
      {(if (isZero (sdiv: (new: Natural 0) 123))
          {(* (new: Natural 9876543) (new: Natural 3456789))}
          {(smod: (new: Natural 3) 0)})}
      {print 'BadComparison})
34141125200427)



;; 3 tests will test the large-integer classes, which are built on 
;; top of class Natural.

; Summary: negative sdiv:/smod:
(check-print
  (if (= (sdiv: (new: LargeInteger 11) -3) (sdiv: (new: LargeInteger -11) 3))
    {(new: LargeInteger (+ (smod: (new: LargeInteger 169) -70) 
                           (smod: (new: LargeInteger -169) 70)))}
    {print 'BadComparison})
0)


; Summary: arithmetic on large numbers, difference signs
(check-print
  (+ (* (asLargeInteger 978990) (asLargeInteger -6894568)) 
     (negated (* (asLargeInteger 978990) (new: LargeInteger -6894568))))
0)

;; Summary:
(check-print 
  (if (= (sdiv: (new: LargeInteger 10) -4) (new: LargeInteger -3)) 
  	{(* (new: LargeInteger -10000) 3456)}
  	{(* (new: LargeInteger -10000) 0)}) 
  -34560000)


;; 3 tests will test mixed arithmetic and comparison involving both small 
;; and large integers.

;; Summary: >, *, -
(check-print 
  (if (> 3 (new: LargeInteger -3)) 
  	{(- (* 100000 (new: LargeInteger 100000)) 1) }
  	{(* 1 (new: LargeInteger 1))}) 
  9999999999)

;; Summary: =, +, -
(check-print 
  (if (= 3 (new: LargeInteger -3)) {(+ 100000 (new: LargeInteger 100000)) }
  	{(+ (- 0 (new: LargeInteger 20000000)) 1)}) 
  -19999999)


;; Summary: <, =, *, -
(check-print 
  (if (< -3 (new: LargeInteger 3)) 
  	{(if (= 0 0) 
  	  {(* (- 100000 (new: LargeInteger 100000)) 300000000)} 
  	  {(* 1 (new: LargeInteger 23984))})}
  	{(+ (- 9929 (new: LargeInteger 2343434)) 2334)}) 
  0)


