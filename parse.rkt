;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname parse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Given

(define-struct Not (arg))
(define-struct And (left right))
(define-struct Or (left right))
(define-struct Implies (left right))
(define-struct If (test conseq alt))

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
