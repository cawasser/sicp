(ns sicp.chapter.1
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SICP Chapter 1 examples
;
;    https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html
;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1 The Elements of Programming
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1.1 Expressions
;

(+ 137 349)  ; 486
(- 1000 334) ; 666
(* 5 99)     ; 495
(/ 10 5)     ; 2
(+ 2.7 10)   ;12.7

(+ 21 35 12 7) ; 75
(* 25 4 12)    ; 1200

(+ (* 3 5) (- 10 6))   ; 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) ; 57

(+                                           ; 57
   (* 3 
      (+ 
         (* 2 4) 
         (+ 3 5))) 
   (+ (- 10 7) 
      6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1.2  Naming and the Environment
;

(def size 2)

size        ; 2
(* 5 size)  ; 10

(def pi 3.14159)
(def radius 10)
(* pi (* radius radius)) ; 314.159

(def circumference (* 2 pi radius))
circumference  ; 62.8318


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1.3  Evaluating Combinations
;

(* (+ 2 (* 4 6))     ; 390   
   (+ 3 5 7))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1.4  Compound Procedures
;

(defn square [x] (* x x))

(square 21)          ; 441
(square (+ 2 5))     ; 49
(square (square 3))  ; 81

(defn sum-of-squares [x y]  
  (+ (square x) (square y)))
(sum-of-squares 3 4)    ; 25

(defn f [a]  
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)   ; 136


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 1.1.5  The Substitution Model for Procedure Application
;



; byrd's Lisp Interpreter 

5 ; 5

(list 5) ; (5)

(list 5 6) ; (5 6)

(list (list 5) 6)

; lists of lists are called "trees"
; recursion!

#t ; true
#f ; false

(quote 5)

(quote (5 6))

(quote ())

'(5 6) ; (5 6)

'milkshake ; an atomic value, a symbol

(null? '()) ; #t

(null? 5) ; #f

(cons 5 '())

(cons 5 6)

(cons 6 (cons 5))

(if (null? 5)
	6
	7)

(cond 
	[(null? 5) 6]
	[(number? #f) 7]
	[else 8])



(require 'clojure.core.match)
; we really condt need core.match, but it makes the more complex expressions easier to write

(defn eval-expr []
	(fn [expr]
		(match expr
			[,n (guard (number? n)] n
			[(add1 ,e)] (add1 (eval-expr e))
		)
	))
	

(eval-expr '5)




