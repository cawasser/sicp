;; gorilla-repl.fileformat = 1

;; **
;;; # Lecture 4A, Pattern Matching: Rule-based Substitution 
;;; 
;;; [Video](https://www.youtube.com/watch?v=amf5lTZ0UTc&index=7&list=PLB745DA2483BEE9C4)
;;; 
;;; Lecturer: Sussman 
;; **

;; @@
(ns sicp.lectures.4A
  (:require [gorilla-plot.core :as plot]
            [sicp.lecture.3A :as s3a]
            [sicp.lecture.3b :as s3b]))

(require 'sicp.lectures.4A)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Part 1
;;; 
;;; In [Lecture 3B](src/sicp/lecture/3b.clj) we built a function that could compute the derivative of a (simplified) symbolc expression. On the upside, it worked and when combined with the algabraic simplification embedded in (make-sum ...) and (make-prod ...), it produced pretty good answeers.
;;; 
;;; On the downside, it could **only** do derivatives, nothing else.
;;; 
;;; It would be very cool if we could generalize the solution so we could perform any type of manipulation on symbolic expressions, even combining mulitple type of conversions into one.
;;; 
;;; 
;; **

;; @@
(defn foo [x]
  [x x])


(defn bar [x y]
  {x y})

(foo 5)
(bar :x 5)
(bar 10 :y)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-keyword'>:y</span>","value":":y"}],"value":"[10 :y]"}],"value":"{10 :y}"}
;; <=

;; **
;;; This mean we ned to parameterize our "symbol manipulator" with the rules for substitution themeslves (which we hard coded in 3B).
;;; 
;;; ### Rules
;;; 
;;; The calculus rules have a left-hand side and a right-hand side:
;;; 
;;; |              | Left hand | Right Hand     |
;;; |--------------|:---------:|:--------------:|
;;; |              | Pattern   | Skeleton       |
;;; | Calc #1      | dc/dx     | 0              |
;;; | Calc #2      | dx/dx     | 1              |
;;; | Calc #3      | d(v+u)/dx | dv/dx + du/dx  |
;;; | Calc #4	   |dcu/dx     | c(du/dx)       |
;;; | Clac #5      | duv/dx    | u(dv/dx) + v(du/dx) |
;;; | etc...       |  etc...   | etc....        |
;;; 
;;; 
;;; 
;;; We match the current expression to a rule, then take the skeleton and instantiate it, substituting knowns for the "template slots", creating a new expression (the "target") and replacing the original (the "source").
;;; 
;;; 
;;; So how do we make these rules?
;;; 
;;; 
;;; Let's write a "language" for the rules, and then an intepreter that can apply them. Bring the computer up to the level of this language, not the other way around. This way we can extend the "machine" with new rules, rather than writing a new program.
;;; 
;;; 
;;; > NOTE: because ":" is special in Clojure, we can't use it in our pattern language (the compiler throws an error) so we need to use something else, let's use "$"
;;; 
;;; > ALSO NOTE: Sussman's changing of the symbols (u, v & c) to (x1, x2, etc) is really confusing
;;; 
;;; 
;;; 
;; **

;; @@
(def deriv-rules 
  '(
     ( (dd (?c c) (? v))       0)       ; rule #1
     ( (dd (?v v) (? v))       1)       ; rule #2
     ( (dd (?v u) (? v))       0)       ; rule #1a
     
     ( (dd (+ (? x1) (? x2)) (? v))     ; rule #3 (i think)
       (+ (dd ($ x1) ($ v))
          (dd ($ x2) ($ v))))

     ( (dd (* (? x1 (? x2))) (? v))       ; left-side
       (+ (* ($ x1) (dd ($ x2) ($ v)))
          (* (dd ($ x1) ($ v)) ($ x2)))) ; right-side
     
     ( (dd (** (? x1) (?c n) (? v))       ; left side
           (* (* ($ n)                    ; right-side
                 (** ($ x) ($ (- n 1))))
              (dd ($ x) ($ v)))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/deriv-rules</span>","value":"#'sicp.lectures.4A/deriv-rules"}
;; <=

;; **
;;; Yes, the syntax is pretty verbose and ugly compared to the math notiation, but the data we need is here.
;;; 
;;; #### Pattern notation
;;; 
;;; | symbol           	| meaning				|
;;; |:----------------:	|:------------------:	|
;;; | (*left* *right*) 	| (pattern skeleton)	|
;;; | dd               	|  derivative-of      	|
;;; | (?a a)           	| (pattern name)	|
;;; |   ?c    		   	| constant            	|
;;; | ?v				| typed-variable		|
;;; | ?        			| untyped-variable      |
;;; | $					| skeleton "hole"		|
;;; 
;;; 
;;; 
;;; for example:
;;; 
;;; |   pattern    | meaning |
;;; |:------------:|:-------------:|
;;; | foo     | foo |
;;; | (f a b) | any '(f a b) (ie., an exact match) |
;;; | (? x)   | **anything** (like an expression) which we will call "x" |
;;; | (?c x)  | any _constant_ which we will call "x" |
;;; | (?v x)  | any variable (non-expression) which we wil call "x" |
;;; 
;;; 
;;; It is important to note that for a given pattern, the value names are bound just once, much like in Logic Programming (which uses a form of pattern-matching)
;;; 
;;; For example, if we have the rule `(dd (?v a) (? a))` then whatever variable is bound to "a" must be the same in _both_ parts of the pattern for the patern to actually match.
;;; 
;;; >for example:
;;; 
;;; | expression | result |
;;; |:----------:|:------:|
;;; | (x x)      | matches |
;;; | (y x)      | does **not** match |
;;; 
;;; 
;;; 
;;; 
;;; 
;;; #### Skeleton notation:
;;; 
;;; |   pattern    | substitution |
;;; |:------------:|:-------------:|
;;; | foo     | foo (itself) |
;;; | (f a b) | '((_instantiate_ f) (_instantiate_ a) (_instantiate_ b)) |
;;; | ($ x) | value of x, as bound in the pattern |
;;; 
;;; 
;;; 
;;; ### Simplifier
;;; 
;;; Now we can start to write a general purpose simplifier, using these rules:
;; **

;; @@
(defn simplifier [rules]
 (fn [expr]
   "Usage: (f '(expr))"))

    
    ; use the rules to simplify the expr (here is the wishful thinking!)
    



(def dsimp (simplifier deriv-rules)) ; dsimp will be a function that can simplify expression using the deriv-rules - this is like a partial (or curry)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/dsimp</span>","value":"#'sicp.lectures.4A/dsimp"}
;; <=

;; **
;;; So we would use dsimp like this:
;;; 
;;; ```
;;; (dsimp '(dd (+ x y) x))
;;;     => (+ 1 0)
;;; ```
;;; 
;;; which is `dx/dx + dy/dx`, which reduces to `1 + 0`
;;; 
;;; >Note the lack of algebraic simplification (guess what? we could add it with more rules!)
;; **

;; @@
(def algebra-rules
  '(
     ( ((? op) (?c e1) (?c e2))
       ($ (op e1 e2)))  ; (op c1 c2)  -> (apply op c1 c2)
     
     ( ((? op) (? e1) (?c e2))
       (($ op) ($ e2) ($ e1)))  ; (op e c)    -> '(op c e)
     
     ( (+ 0 (? e))                    ($ e))  ; (+ 0 e)     -> e
      
     ( (* 1 (? e))                    ($ e))  ; (* 1 e)     -> e
     
     ( (* 0 (? e))                    0)  ; (* 0 e)     -> 0
     
     ( (* (? c1) (* (? c2) (? e)))    (* (* c1 c2) ($ e)))))  ; 3(3x)       -> 9x
     
     ; etc...
     

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/algebra-rules</span>","value":"#'sicp.lectures.4A/algebra-rules"}
;; <=

;; **
;;; ### Interpretation
;;; 
;;; Now we can look at how to interpret the rules. Sussman drew it this way:
;;; 
;;; [link](resources/4a-screenshot-1.png)
;;; 
;;; 
;;; >NOTE: A key point is that as substitutions are made, we need to revist all the rules then too, so that we get the maximium reduction all at once, otherwisse we'd end up having to walk the exression tree multiple times.
;;; 
;;; >> Note to the note: This means we can find ourselves in an infinte loop is we aren't careful in developing our rules.
;;; 
;;; 
;;; ## Part 2
;;; 
;;; 
;;; The _Simplifier_ will be a 2-step process "Match" 
;;; 
;;; ```                 
;;; expression ----->|       |
;;;                  |       |
;;; pattern -------->| MATCH |-------> dictionary'
;;;                  |       |
;;; dictionary ----->|       |
;;; ```
;;; 
;; **

;; @@
(s3b/atomic? 'c)
(s3b/atomic? '(+ x y))


;; @@

;; @@
(s3b/eq? 'x '3)
(s3b/eq? 'x 'x)
(s3b/eq? '3 '3)
;; @@

;; @@
(defn handle-atoms [pat expr dict]
  (if (s3b/atomic? expr)        ; if the expr is atomic
    (if (s3b/eq? pat expr)      ; then if the pat and expr match,
      dict                      ;         then done (no more substitutions to do)
      :failed)                  ;         else we didn't match
    :failed))                     ; else we didn't match

 

(handle-atoms 'x '3 {})
(handle-atoms '3 '3 {:x 3})
(handle-atoms '(+ 1 x) '(+ 1 6) {})
 
;; @@

;; @@
(defn extend-dict [pat expr dict]
  dict)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/extend-dict</span>","value":"#'sicp.lectures.4A/extend-dict"}
;; <=

;; @@
(defn arbitray-constant? [pat])


(defn constant? [expr])


(defn handle-constant [pat expr dict]
  (if (constant? expr)
    (extend-dict pat expr dict)      ; we found a match, so bind the name
    :failed))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/handle-constant</span>","value":"#'sicp.lectures.4A/handle-constant"}
;; <=

;; @@
(defn arbitrary-variable? [pat])


(defn variable? [expr])


(defn handle-variables [pat expr dict]
  (if (variable? expr)
    (extend-dict pat expr dict)
    :failed))

  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/handle-variables</span>","value":"#'sicp.lectures.4A/handle-variables"}
;; <=

;; @@
(defn arbitrary-expression? [pat])


(defn expression? [expr])


(defn handle-expression [pat expr dict]
    (extend-dict pat expr dict))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lectures.4A/handle-expression</span>","value":"#'sicp.lectures.4A/handle-expression"}
;; <=

;; @@
 
(defn match [pat expr dict]
  (cond
    (= dict :failed) :failed                ; the pattern does NOT match!
    (s3b/atomic? pat)
    (handle-atoms pat expr dict)         ; handle atomic patterns
     
    (arbitray-constant? pat)
    (handle-constant pat expr dict)     ; handle an constants
     
    (arbitrary-variable? pat)
    (handle-variables pat expr dict)     ; handle variables
     
    (arbitrary-expression? pat)
    (handle-expression pat expr dict)    ; handle expressions
     
    (s3b/atomic? expr) :failed))              ;
  ;   :else (match (rest pat)                 ; presess the rest of the pattern
  ;                (rest expr)
  ;                (match (first pat)         ; after first processing the left-hand child
  ;                       (first expr)
  ;                       dict))        ; this part is a tree-walk down the pattern-tree and the expression-tree

;; @@

;; @@
(match 'x '1 {})
(match '1 '1 {})
(match 5 5 {:x 5})
(match 'x 'x :failed)
(match '(+ x y) 'x {})
(match 'x '(+ 1 3) {:x 3})
;; @@

;; **
;;;  and "Substitute"
;;; 
;;; 
;;; 
;;; 
;;; 
;; **

;; @@

;; @@
