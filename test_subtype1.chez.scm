(include "mk-vicare.scm")
(include "mk.scm")
;(include "../simple-miniKanren/test-check.scm")
(include "list-display.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define contravariant-subtype (lambda (a b)
  (conde 
    ( (is-reference b)      (subtype b a)       )
    ( (is-valuetype b) (== b a) )
)))

(define is-reference (lambda (a)
  (conde 
    ((fresh (q) 
       (== a `(ibox1 ,q)) ) )
    
    ( (== a 'object3) )
    ( (== a 'thing2) )
    ( (== a a) )
)))

(define is-valuetype (lambda (a) fail ))

(define not-contravariant-subtype (lambda (a b)
  (conde 
    ( (is-reference b) (not-subtype b a))
    ( (is-valuetype b) (=/= b a) )
)))

(define not-subtype (lambda (t st)
  (conde 
    ( (fresh (q) 
	(== t `(ibox1 ,q))
	(== st 'thing2) 
      ))
    ( (fresh (a b)
	(== t  `(ibox1 ,a))
	(== st `(ibox1 ,b))
	(not-contravariant-subtype a b)   
	))
    ( (fresh (q)     
        (== t  'object3)
	(== st `(ibox1 ,q))	) )
    ( (== t 'object3) (== st 'thing2) )
    ( (fresh (a)
        (not-subtype `(ibox1 (ibox1 thing2)) `(ibox1 ,a) )
	(== t  'thing2)
	(== st `(ibox1 ,a)) ))
)))
 

(define subtype (lambda (t st)
  (conde
    ( (fresh (a)
        (== st 'object3) 
        (== t `(ibox1 ,a)) ))
    ( (fresh (a b)
 	 (== t  `(ibox1 ,a))
 	 (== st `(ibox1 ,b))
	 (contravariant-subtype a b)    ))
    ( (== t 'object3) (== st t) )
    ( (fresh (a)
        (== t 'thing2) 
        (== st `(ibox1 ,a))    
        (subtype `(ibox1 (ibox1 thing2)) `(ibox1 ,a))  ))
    ( (== t 'thing2)  (== st 'object3) )     
    ( (== t 'thing2)  (== st t) )
  )
))

;(list-display
;  (run 10 (a )
;    (subtype a '(ibox1 thing2)))  ))

(printf "asdfasd\n")
(list-display
  (run 10 (a )
    (not-subtype a '(ibox1 thing2))
  )
)

