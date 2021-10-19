;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;COMP 311 HW #05
;; Luis Leal <lal10@rice.edu>
I used 1 slip day

(define-struct Not (arg))
(define-struct And (left right))
(define-struct Or (left right))
(define-struct Implies (left right))
(define-struct If (test conseq alt))
(define-struct binding (s v))

;; a BoolExp is either
;; * a constant true or false,
;; * a variable S,
;; * a Not form (make-Not X),
;; * an And form (make-And X Y),
;; * an Or form (make-Or X Y),
;; * an Implies form (make-Implies X Y),
;; * an If form (make-If X Y Z),
;; where S is a symbol (other than a keyword) and X, Y, and Z are BoolExps.

;;A BoolRacketExp is either:
;;* a boolean constant true or false;;
;;* a symbol;;
;;* (list 'not X) where X is a BoolRacketExp;;
;;* (list op X Y) where op is 'and 'or 'implies where X and Y are BoolRacketExps;;
;;* (list 'if X Y Z) where X, Y, and Z are BoolRacketExps

;;parse: BoolRacketExp -> BoolExp
;;Purpose: (parse bse) returns the BoolExp corresponding to the BoolRacketExp bse

;;Examples
(check-expect (parse true) true)
(check-expect (parse 'x) 'x)
(check-expect (parse '(and x y)) (make-And 'x 'y))
(check-expect (parse '(or x y)) (make-Or 'x 'y))
(check-expect (parse '(implies x y)) (make-Implies 'x 'y))
(check-expect (parse '(if x y z)) (make-If 'x 'y 'z))
(check-expect (parse '(if x true false)) (make-If 'x true false))
(check-expect (parse '(not true)) (make-Not true))
                                        
;; Template instantiation
#|
(define (parse bse)
  (cond [(boolean? bse) ...]
        [(symbol? bse) ...]
        [else 
         (local [(define head (first bse))]
           (cond [(equal? head not) ...]
                 [(equal? head 'and) ...]
                 [(equal? head 'or) ...]
                 [(equal? head 'implies) ...]
                 [(equal? head 'if) ...]))]))
|#
(define (parse bse)
  (cond [(boolean? bse) bse]
        [(equal? bse 'true) true]
        [(equal? bse 'false) false]
        [(symbol? bse) bse]
        [else 
         (local [(define head (first bse))
                 (define args (rest bse))
                 (define val1 (parse (first args)))]
           (cond [(equal? head 'not) (make-Not val1)]
                 [else 
                  (local [(define val2 (parse (first (rest args))))]
                    (cond [(equal? head 'and) (make-And val1 val2)]
                          [(equal? head 'or) (make-Or val1 val2)]
                          [(equal? head 'implies) (make-Implies val1 val2)]
                          [(equal? head 'if) (make-If val1 val2 (parse (first (rest (rest args)))))]))]))]))

;;parse: BoolExp -> BoolRacketExp
;;Purpose: (parse bse) returns the BoolExp corresponding to the BoolRacketExp bse

;; Examples
(check-expect (unparse true) true)
(check-expect (unparse 'x) 'x)
(check-expect (unparse (make-Not 'x)) '(not x))
(check-expect (unparse (make-And 'x 'y)) '(and x y))
(check-expect (unparse (make-Or 'x 'y)) '(or x y))
(check-expect (unparse (make-Implies 'x 'y)) '(implies x y))
(check-expect (unparse (make-If 'x 'y 'z)) '(if x y z))
           
;; Template Instantiation
#|
(define (unparse be)
  (cond [(equal? be true) ...]
        [(equal? be false) ...]
        [(symbol? be) ...]
        [(Not? be) ... (unparse (Not-arg be)) ...]
        [(And? be) ... (unparse (And-left be))
                  ... (unparse (And-right be)) ...]
        [(Or? be) ... (unparse (Or-left be))
                  ... (unparse (Or-right be)) ...]
        [(Implies? be) ... (unparse (Implies-left be))
                       ... (unparse (Implies-right be)) ...]
        [else ;; (must be (make-If be1 be2 be3))
         ... (unparse (If-test be))
         ... (unparse (If-conseq be)) 
         ... (unparse (If-alt be)) ...]
|#
(define (unparse be)
  (cond [(boolean? be) be]
        [(symbol? be) be]
        [(Not? be) (list 'not (unparse (Not-arg be)))]
        [(And? be) (list 'and (unparse (And-left be)) (unparse (And-right be)))]
        [(Or? be)  (list 'or  (unparse (Or-left be))  (unparse (Or-right be)))]
        [(Implies? be) (list 'implies (unparse (Implies-left be)) (unparse (Implies-right be)))]
        [(If? be) (list 'if (unparse (If-test be)) (unparse (If-conseq be)) (unparse (If-alt be)))]))


;; Problem 1                       
;; convertToIf:  BoolExprs -> make-If 
;;Purpose: (convertToIf) returns a boolean expression where the only constructor is make-If

;; Examples
(check-expect (convertToIf (make-Or (make-And 'x 'y) 'z)) (make-If (make-If 'x 'y false) true 'z))
(check-expect (convertToIf (make-Implies 'x (make-Not 'y))) (make-If 'x (make-If 'y false true) true))
(check-expect (convertToIf (make-Not 'X))(make-If 'X false true))
(check-expect (convertToIf (make-And 'X 'Y))(make-If 'X 'Y false))
(check-expect (convertToIf (make-Or 'X 'Y))(make-If 'X true 'Y))

;; Template Instantiation
#|
(define (convertToIf boolExpr) 
  (cond [(Not? boolExpr) ....] 
        [(And? boolExpr) ...]
        [(Or? boolExpr) ....]
        [(Implies? boolExpr) ...]
        [... boolExpr]))
|#

;; Code
(define (convertToIf boolExpr) 
  (cond [(Not? boolExpr) (make-If (convertToIf (Not-arg boolExpr)) false true)] 
        [(And? boolExpr) (make-If (convertToIf (And-left boolExpr)) (convertToIf (And-right boolExpr)) false)]
        [(Or? boolExpr) (make-If (convertToIf (Or-left boolExpr)) true (convertToIf (Or-right boolExpr)))]
        [(Implies? boolExpr) (make-If (convertToIf (Implies-left boolExpr)) (convertToIf (Implies-right boolExpr)) true)]
        [else boolExpr]))

;; Test Cases
(check-expect (convertToIf (make-Or (make-And 'x 'y) 'z)) (make-If (make-If 'x 'y false) true 'z))
(check-expect (convertToIf (make-Implies 'x (make-Not 'y))) (make-If 'x (make-If 'y false true) true))
(check-expect (convertToIf (make-Not 'X))(make-If 'X false true))
(check-expect (convertToIf (make-And 'X 'Y))(make-If 'X 'Y false))
(check-expect (convertToIf (make-Or 'X 'Y))(make-If 'X true 'Y))


;; Problem 2
;; head-normalize: X Y Z -> NormIfExp
;;Purpose: (head-normalize) constructs a NormIfExp equivalent to (makeIf X Y Z) 

;; Examples
(check-expect (head-normalize 'x 'y 'z) (make-If 'x 'y 'z))
(check-expect (head-normalize true 'y 'z) (make-If true 'y 'z))
(check-expect (head-normalize false 'y 'z) (make-If false 'y 'z))
(check-expect (head-normalize (make-If 'x 'y 'z) 'u 'v) (make-If 'x (make-If 'y 'u 'v) (make-If 'z 'u 'v)))
(check-expect (head-normalize (make-If 'x (make-If 'yt 'yc 'ya) (make-If 'zt 'zc 'za)) 'u 'v) (make-If 'x (make-If 'yt (make-If 'yc 'u 'v) (make-If 'ya 'u 'v)) (make-If 'zt (make-If 'zc 'u
'v) (make-If 'za 'u 'v))))

;; Template Instantiation
#|
(define (head-normalize x y z)
  (cond [(If? x) (head-normalize (If-test x) ...]
        [(and (If? y)(If? (If-test y))) ...]
        [(and (If? z)(If? (If-test z))) ...]
        [... (make-If x y z)])))
|#

; Code
(define (head-normalize x y z)
  (cond [(If? x) (head-normalize (If-test x) (make-If (If-conseq x) y z) (make-If (If-alt x) y z))]
        [(and (If? y)(If? (If-test y))) (head-normalize x (head-normalize (If-test y) (If-conseq y) (If-alt y)) z)]
        [(and (If? z)(If? (If-test z))) (head-normalize x y (head-normalize (If-test z) (If-conseq z) (If-alt z)))]
        [else (make-If x y z)]))

;; Test Cases
(check-expect (head-normalize 'x 'y 'z) (make-If 'x 'y 'z))
(check-expect (head-normalize true 'y 'z) (make-If true 'y 'z))
(check-expect (head-normalize false 'y 'z) (make-If false 'y 'z))
(check-expect (head-normalize (make-If 'x 'y 'z) 'u 'v) (make-If 'x (make-If 'y 'u 'v) (make-If 'z 'u 'v)))
(check-expect (head-normalize (make-If 'x (make-If 'yt 'yc 'ya) (make-If 'zt 'zc 'za)) 'u 'v) (make-If 'x (make-If 'yt (make-If 'yc 'u 'v) (make-If 'ya 'u 'v)) (make-If 'zt (make-If 'zc 'u 'v) (make-If 'za 'u 'v))))

    

;; normalize: expr -> NormExpr 
;;Purpose: (normalize) returns a sub-expression where the test position is either a variable or a constant (true or false)

;; Helper function 1 for normalize
;; normalize: expr -> NormExpr 
;;Purpose: (normalize2) returns a sub-expression normalized given (If-conseq expr) is normalized
(define (normalize2 expr)
  (cond [(and (If? expr) (If? (If-test expr))) (head-normalize (normalize (If-test expr)) (If-conseq expr) (If-alt expr))]
        [(If? expr) (head-normalize (If-test expr)(If-conseq expr)(If-alt expr))]))

;;Helper function 2 for normalize
;(there was 2 helper functions with mostly the same code since recursion was in a never ending loop, sorry about that. It works tho)
;; normalize2: expr -> NormExpr 
;;Purpose: (normalize1) returns a sub-expression where the test position is either a variable or a constant (true or false)
(define (normalize1 expr)
  (cond [(and (If? expr) (If? (If-alt expr))) (normalize2 (make-If (If-test expr)(If-conseq expr)(normalize (If-alt expr))))]
        [(and (If? expr) (If? (If-test expr))) (head-normalize (normalize (If-test expr)) (If-conseq expr) (If-alt expr))]
        [(If? expr) (head-normalize (If-test expr)(If-conseq expr)(If-alt expr))]))

;; Examples for normalize
(check-expect (normalize true) true)
(check-expect (normalize false) false)
(check-expect (normalize 'x) 'x)
(check-expect (normalize (make-If 'x 'y 'z)) (make-If 'x 'y 'z))
(check-expect (normalize (make-If (make-If 'x 'y 'z) 'u 'v)) (make-If 'x (make-If 'y 'u 'v)
(make-If 'z 'u 'v)))
(check-expect (normalize (make-If 'x (make-If (make-If 1 2 3) 'b 'c) 'z)) (make-If 'x (make-If 1 (make-If 2 'b 'c) (make-If 3 'b 'c)) 'z))

;; Template Instantiation
#|
;; Code
(define (normalize expr)
  (cond [(and (If? expr) (If? (If-conseq expr))) ...]
        [(and (If? expr) (If? (If-alt expr))) ....]
        [(and (If? expr) (If? (If-test expr))) ...]
        [(or (boolean? expr)(symbol? expr)) ...]
        [(If? expr) ...]))
|#

;; Code
(define (normalize expr)
  (cond [(and (If? expr) (If? (If-conseq expr))) (normalize1 (make-If (If-test expr)(normalize (If-conseq expr))(If-alt expr)))]
        [(and (If? expr) (If? (If-alt expr))) (normalize2 (make-If (If-test expr)(If-conseq expr)(normalize (If-alt expr))))]
        [(and (If? expr) (If? (If-test expr))) (head-normalize (normalize (If-test expr)) (If-conseq expr) (If-alt expr))]
        [(or (boolean? expr)(symbol? expr)) expr]
        [(If? expr) (head-normalize (If-test expr)(If-conseq expr)(If-alt expr))]))

;; Test Cases
(check-expect (normalize true) true)
(check-expect (normalize false) false)
(check-expect (normalize 'x) 'x)
(check-expect (normalize (make-If 'x (make-If 1 2 3) (make-If 4 5 6))) (make-If 'x (make-If 1 2 3) (make-If 4 5 6)))
(check-expect (normalize (make-If 'x 'y 'z)) (make-If 'x 'y 'z))
(check-expect (normalize (make-If (make-If 'x 'y 'z) 'u 'v)) (make-If 'x (make-If 'y 'u 'v)(make-If 'z 'u 'v)))
(check-expect (normalize (make-If 'x (make-If (make-If 1 2 3) 'b 'c) 'z)) (make-If 'x (make-If 1 (make-If 2 'b 'c) (make-If 3 'b 'c)) 'z))
(check-expect (normalize (make-If (make-If (make-If 1 2 3) 'a 'b) (make-If (make-If 4 5 6) 'c 'd) (make-If (make-If 7 8 9) 'e 'f)))
              (make-If 1 (make-If 2 (make-If 'a (make-If 4 (make-If 5 'c 'd) (make-If 6 'c 'd)) (make-If 7 (make-If 8 'e 'f) (make-If 9 'e 'f))) (make-If 'b (make-If
              4 (make-If 5 'c 'd) (make-If 6 'c 'd)) (make-If 7 (make-If 8 'e 'f) (make-If 9 'e 'f))))(make-If 3 (make-If 'a (make-If 4 (make-If 5 'c 'd) (make-If 6 'c 'd))
              (make-If 7 (make-If 8 'e 'f) (make-If 9 'e 'f))) (make-If 'b (make-If 4 (make-If 5 'c 'd) (make-If 6 'c 'd)) (make-If 7 (make-If 8 'e 'f) (make-If 9 'e 'f))))))



;; Helper functions for Problem 3
;; contains1: list x -> boolean 
;;Purpose: (contains1) returns a boolean depending on whether x is found in the binding-list environment 'list' 
(define (contains1 list x)
	(cond [(null? list) #f]
		[(equal? (binding-s (car list)) x) #t]
		[else (contains1 (cdr list) x)]))
;; contains2: list x -> v 
;;Purpose: (contains2) returns 'v' using x in the make-binding list found in the environment 'list', it follows that it has already been identified as found in the list
(define (contains2 list x)
	(cond [(equal? (binding-s (car list)) x) (binding-v (car list))]
              [else (contains2 (cdr list) x)]))


;; Problem 3
;; eval: NormIfExp environment -> NormIfExp
;;Purpose: (eval) reduces a NormIfExp to simple form

;; Examples
(check-expect (eval (make-If true 'X 'Y) (list)) 'X)
(check-expect (eval (make-If false 'X 'Y) (list)) 'Y)
(check-expect (eval (make-If 'X true false) (list)) 'X)
(check-expect (eval (make-If 'X 'Y 'Y) (list)) 'Y)
(check-expect (eval (make-If true 'Y 'Z)(list)) 'Y)
(check-expect (eval (make-If false 'Y 'Z) (list)) 'Z)

;; Template Instantiation
#|
(define (eval NormIfExp environment)
  (cond[(and (If? NormIfExp)(equal? true (If-test NormIfExp))) ...]
       [(and (If? NormIfExp)(equal? false (If-test NormIfExp)))...]
       [(and (If? NormIfExp)(symbol? (If-test NormIfExp))(contains1 environment (If-test NormIfExp))) ...]
       [(and (If? NormIfExp)(equal? true (If-conseq NormIfExp)) (equal? false (If-alt NormIfExp))) ...]
       [(and (If? NormIfExp)(equal? (If-conseq NormIfExp) (If-alt NormIfExp)) (not (equal? (If-test NormIfExp) (If-conseq NormIfExp)))) ...]
       [(and (If? NormIfExp)(or(If? (If-conseq NormIfExp)) (If? (If-alt NormIfExp)))) ...]
       [(and (symbol? NormIfExp) (contains1 environment NormIfExp))...]
       [... NormIfExp]))
|#

;; Code
(define (eval NormIfExp environment)
  (cond[(and (If? NormIfExp)(equal? true (If-test NormIfExp))) (eval (If-conseq NormIfExp) environment)]
       [(and (If? NormIfExp)(equal? false (If-test NormIfExp))) (eval (If-alt NormIfExp) environment)]
       [(and (If? NormIfExp)(symbol? (If-test NormIfExp))(contains1 environment (If-test NormIfExp))) (eval (make-If (contains2 environment (If-test NormIfExp)) (If-conseq NormIfExp) (If-alt NormIfExp)) environment)]
       [(and (If? NormIfExp)(equal? true (If-conseq NormIfExp)) (equal? false (If-alt NormIfExp))) (eval (If-test NormIfExp) environment)]
       [(and (If? NormIfExp)(equal? (If-conseq NormIfExp) (If-alt NormIfExp)) (not (equal? (If-test NormIfExp) (If-conseq NormIfExp)))) (eval (If-conseq NormIfExp) environment)]
       [(and (If? NormIfExp)(or(If? (If-conseq NormIfExp)) (If? (If-alt NormIfExp)))) (eval (make-If (If-test NormIfExp) (eval (If-conseq NormIfExp) (cons (make-binding (If-test NormIfExp) true) environment)) (eval (If-alt NormIfExp) (cons (make-binding (If-test NormIfExp) false) environment))) environment)]
       [(and (symbol? NormIfExp) (contains1 environment NormIfExp)) (contains2 environment NormIfExp)]
       [else NormIfExp]))

;; Test Cases
(check-expect (eval (make-If true 'X 'Y) (list)) 'X)
(check-expect (eval (make-If false 'X 'Y) (list)) 'Y)
(check-expect (eval (make-If 'X true false) (list)) 'X)
(check-expect (eval (make-If 'X 'Y 'Y) (list)) 'Y)
(check-expect (eval (make-If true 'Y 'Z)(list)) 'Y)
(check-expect (eval (make-If false 'Y 'Z) (list)) 'Z)
(check-expect (eval (make-If true 'X 'Y) (list (make-binding 'X true) (make-binding 'Y false))) true)
(check-expect (eval (make-If false 'X 'Y) (list (make-binding 'X true) (make-binding 'Y false))) false)
(check-expect (eval (make-If 'X true false) (list (make-binding 'X true) (make-binding 'Y false))) true)
(check-expect (eval (make-If 'X 'Y 'Y)(list (make-binding 'X true) (make-binding 'Y false))) false)
(check-expect (eval (make-If true (make-If true 'X false) 'Y) (list)) 'X)
(check-expect (eval (make-If 'X (make-If true true false) (make-If true false false)) (list)) 'X)

    


;; Problem 4
;; convertToBool: ifExpr -> Expr
;;Purpose: (convertToBool) converts an expression in (not necessarily reduced or normalized) If form to an equivalent expression constructed from variables and {true, false, And, Or, Not, Implies, If}. 

;; Examples
(check-expect (convertToBool(make-If 'X false true)) (make-Not 'X))
(check-expect (convertToBool(make-If 'X 'Y false)) (make-And 'X 'Y))
(check-expect (convertToBool(make-If 'X true 'Y)) (make-Or 'X 'Y))
(check-expect (convertToBool(make-If 'X 'Y true)) (make-Implies 'X 'Y))

;; Template Instantiation
#|
(define (convertToBool ifExpr)
  (cond[(and (If? ifExpr) (equal? false (If-conseq ifExpr)) (equal? true (If-alt ifExpr))) ...]
       [(and (If? ifExpr) (equal? false (If-alt ifExpr))) (make-And (convertToBool(If-test ifExpr))...]
       [(and (If? ifExpr) (equal? true (If-conseq ifExpr))) (make-Or (convertToBool (If-test ifExpr))...]
       [(and (If? ifExpr) (equal? true (If-alt ifExpr))) (make-Implies (convertToBool(If-test ifExpr))...]
       [else ifExpr]))


|#

;; Code
(define (convertToBool ifExpr)
  (cond[(and (If? ifExpr) (equal? false (If-conseq ifExpr)) (equal? true (If-alt ifExpr))) (make-Not (convertToBool (If-test ifExpr)))]
       [(and (If? ifExpr) (equal? false (If-alt ifExpr))) (make-And (convertToBool(If-test ifExpr))(convertToBool (If-conseq ifExpr)))]
       [(and (If? ifExpr) (equal? true (If-conseq ifExpr))) (make-Or (convertToBool (If-test ifExpr)) (convertToBool (If-alt ifExpr)))]
       [(and (If? ifExpr) (equal? true (If-alt ifExpr))) (make-Implies (convertToBool(If-test ifExpr))(convertToBool(If-conseq ifExpr)))]
       [else ifExpr]))


;; Test Cases
(check-expect (convertToBool(make-If 'X false true)) (make-Not 'X))
(check-expect (convertToBool(make-If 'X 'Y false)) (make-And 'X 'Y))
(check-expect (convertToBool(make-If 'X true 'Y)) (make-Or 'X 'Y))
(check-expect (convertToBool(make-If 'X 'Y true)) (make-Implies 'X 'Y))
(check-expect (convertToBool(make-If (make-If 'X false true) false true)) (make-Not (make-Not 'X)))



;; Problem 5
;; reduce: boolExp -> simplifiedBoolExpr
;;Purpose: (reduce) reduces boolean expressions (represented in Racket notation) to simplified form 

;; Examples
;; Can be found in test cases

;; Template Instantiation
#|
(define (reduce boolExp)
  (convertToBool (eval (normalize (convertToIf (parse boolExp))) '())))
|#

;; Code
(define (reduce boolExp)
  (convertToBool (eval (normalize (convertToIf (parse boolExp))) '())))


;; Test Cases
(define smallE '(or (and x y)
                    (or (and x (not y))
                        (or (and (not x) y)
                            (and (not x) (not y))))))

(check-expect (reduce smallE) true)

(define smallE2 '(or 
                  (or (and x (not y))
                      (or (and (not x) y)
                          (and (not x) (not y))))
                  (and x y)))

(check-expect (reduce smallE2) true)

(define midE
  '(or (and   v   (and   w   (and   x   (and   y     z  ))))
       (or (and   v   (and   w   (and   x   (and   y   (not z)))))
           (or (and   v   (and   w   (and   x   (and (not y)   z  ))))
               (or (and   v   (and   w   (and   x   (and (not y) (not z)))))
                   (or (and   v   (and   w   (and (not x) (and   y     z  ))))
                       (or (and   v   (and   w   (and (not x) (and   y   (not z)))))
                           (or (and   v   (and   w   (and (not x) (and (not y)   z  ))))
                               (or (and   v   (and   w   (and (not x) (and (not y) (not z)))))
                                   (or (and   v   (and (not w) (and   x   (and   y     z  ))))
                                       (or (and   v   (and (not w) (and   x   (and   y   (not z)))))
                                           (or (and   v   (and (not w) (and   x   (and (not y)   z  ))))
                                               (or (and   v   (and (not w) (and   x   (and (not y) (not z)))))
                                                   (or (and   v   (and (not w) (and (not x) (and   y     z  ))))
                                                       (or (and   v   (and (not w) (and (not x) (and   y   (not z)))))
                                                           (or (and   v   (and (not w) (and (not x) (and (not y)   z  ))))
                                                               (or (and   v   (and (not w) (and (not x) (and (not y) (not z)))))
                                                                   (or (and (not v) (and   w   (and   x   (and   y     z  ))))
                                                                       (or (and (not v) (and   w   (and   x   (and   y   (not z)))))
                                                                           (or (and (not v) (and   w   (and   x   (and (not y)   z  ))))
                                                                               (or (and (not v) (and   w   (and   x   (and (not y) (not z)))))
                                                                                   (or (and (not v) (and   w   (and (not x) (and   y     z  ))))
                                                                                       (or (and (not v) (and   w   (and (not x) (and   y   (not z)))))
                                                                                           (or (and (not v) (and   w   (and (not x) (and (not y)   z  ))))
                                                                                               (or (and (not v) (and   w   (and (not x) (and (not y) (not z)))))
                                                                                                   (or (and (not v) (and (not w) (and   x   (and   y     z  ))))
                                                                                                       (or (and (not v) (and (not w) (and   x   (and   y   (not z)))))
                                                                                                           (or (and (not v) (and (not w) (and   x   (and (not y)   z  ))))
                                                                                                               (or (and (not v) (and (not w) (and   x   (and (not y) (not z)))))
                                                                                                                   (or (and (not v) (and (not w) (and (not x) (and   y     z  ))))
                                                                                                                       (or (and (not v) (and (not w) (and (not x) (and   y   (not z)))))
                                                                                                                           (or (and (not v) (and (not w) (and (not x) (and (not y)   z  ))))
                                                                                                                               (and (not v) (and (not w) (and (not x) (and (not y) (not z)))))))))))))))))))))))))))))))))))))


(check-expect (reduce midE) true)

(define bigE
'(or (and   u   (and   v   (and   w   (and   x   (and   y     z  )))))
(or (and   u   (and   v   (and   w   (and   x   (and   y   (not z))))))
(or (and   u   (and   v   (and   w   (and   x   (and (not y)   z  )))))
(or (and   u   (and   v   (and   w   (and   x   (and (not y) (not z))))))
(or (and   u   (and   v   (and   w   (and (not x) (and   y     z  )))))
(or (and   u   (and   v   (and   w   (and (not x) (and   y   (not z))))))
(or (and   u   (and   v   (and   w   (and (not x) (and (not y)   z  )))))
(or (and   u   (and   v   (and   w   (and (not x) (and (not y) (not z))))))
(or (and   u   (and   v   (and (not w) (and   x   (and   y     z  )))))
(or (and   u   (and   v   (and (not w) (and   x   (and   y   (not z))))))
(or (and   u   (and   v   (and (not w) (and   x   (and (not y)   z  )))))
(or (and   u   (and   v   (and (not w) (and   x   (and (not y) (not z))))))
(or (and   u   (and   v   (and (not w) (and (not x) (and   y     z  )))))
(or (and   u   (and   v   (and (not w) (and (not x) (and   y   (not z))))))
(or (and   u   (and   v   (and (not w) (and (not x) (and (not y)   z  )))))
(or (and   u   (and   v   (and (not w) (and (not x) (and (not y) (not z))))))
(or (and   u   (and (not v) (and   w   (and   x   (and   y     z  )))))
(or (and   u   (and (not v) (and   w   (and   x   (and   y   (not z))))))
(or (and   u   (and (not v) (and   w   (and   x   (and (not y)   z  )))))
(or (and   u   (and (not v) (and   w   (and   x   (and (not y) (not z))))))
(or (and   u   (and (not v) (and   w   (and (not x) (and   y     z  )))))
(or (and   u   (and (not v) (and   w   (and (not x) (and   y   (not z))))))
(or (and   u   (and (not v) (and   w   (and (not x) (and (not y)   z  )))))
(or (and   u   (and (not v) (and   w   (and (not x) (and (not y) (not z))))))
(or (and   u   (and (not v) (and (not w) (and   x   (and   y     z  )))))
(or (and   u   (and (not v) (and (not w) (and   x   (and   y   (not z))))))
(or (and   u   (and (not v) (and (not w) (and   x   (and (not y)   z  )))))
(or (and   u   (and (not v) (and (not w) (and   x   (and (not y) (not z))))))
(or (and   u   (and (not v) (and (not w) (and (not x) (and   y     z  )))))
(or (and   u   (and (not v) (and (not w) (and (not x) (and   y   (not z))))))
(or (and   u   (and (not v) (and (not w) (and (not x) (and (not y)   z  )))))
(or (and   u   (and (not v) (and (not w) (and (not x) (and (not y) (not z))))))
(or (and (not u) (and   v   (and   w   (and   x   (and   y     z  )))))
(or (and (not u) (and   v   (and   w   (and   x   (and   y   (not z))))))
(or (and (not u) (and   v   (and   w   (and   x   (and (not y)   z  )))))
(or (and (not u) (and   v   (and   w   (and   x   (and (not y) (not z))))))
(or (and (not u) (and   v   (and   w   (and (not x) (and   y     z  )))))
(or (and (not u) (and   v   (and   w   (and (not x) (and   y   (not z))))))
(or (and (not u) (and   v   (and   w   (and (not x) (and (not y)   z  )))))
(or (and (not u) (and   v   (and   w   (and (not x) (and (not y) (not z))))))
(or (and (not u) (and   v   (and (not w) (and   x   (and   y     z  )))))
(or (and (not u) (and   v   (and (not w) (and   x   (and   y   (not z))))))
(or (and (not u) (and   v   (and (not w) (and   x   (and (not y)   z  )))))
(or (and (not u) (and   v   (and (not w) (and   x   (and (not y) (not z))))))
(or (and (not u) (and   v   (and (not w) (and (not x) (and   y     z  )))))
(or (and (not u) (and   v   (and (not w) (and (not x) (and   y   (not z))))))
(or (and (not u) (and   v   (and (not w) (and (not x) (and (not y)   z  )))))
(or (and (not u) (and   v   (and (not w) (and (not x) (and (not y) (not z))))))
(or (and (not u) (and (not v) (and   w   (and   x   (and   y     z  )))))
(or (and (not u) (and (not v) (and   w   (and   x   (and   y   (not z))))))
(or (and (not u) (and (not v) (and   w   (and   x   (and (not y)   z  )))))
(or (and (not u) (and (not v) (and   w   (and   x   (and (not y) (not z))))))
(or (and (not u) (and (not v) (and   w   (and (not x) (and   y     z  )))))
(or (and (not u) (and (not v) (and   w   (and (not x) (and   y   (not z))))))
(or (and (not u) (and (not v) (and   w   (and (not x) (and (not y)   z  )))))
(or (and (not u) (and (not v) (and   w   (and (not x) (and (not y) (not z))))))
(or (and (not u) (and (not v) (and (not w) (and   x   (and   y     z  )))))
(or (and (not u) (and (not v) (and (not w) (and   x   (and   y   (not z))))))
(or (and (not u) (and (not v) (and (not w) (and   x   (and (not y)   z  )))))
(or (and (not u) (and (not v) (and (not w) (and   x   (and (not y) (not z))))))
(or (and (not u) (and (not v) (and (not w) (and (not x) (and   y     z  )))))
(or (and (not u) (and (not v) (and (not w) (and (not x) (and   y   (not z))))))
(or (and (not u) (and (not v) (and (not w) (and (not x) (and (not y)   z  )))))
   (and (not u) (and (not v) (and (not w) (and (not x) (and (not y) (not z))))))
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
)

(check-expect (reduce bigE) true)
