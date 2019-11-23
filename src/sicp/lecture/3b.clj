;; gorilla-repl.fileformat = 1

;; **
;;; #Lecture 3B
;;; 
;;; 
;;; SICP Lecture 3b examples
;;; 
;;;    [video](https://www.youtube.com/watch?v=X21cKVtGvYk&list=PLB745DA2483BEE9C4&index=6)
;;; 
;;; Lecturer: Sussman
;;; 
;;; 
;;; 
;;; 
;;; ## Last Time...
;;; 
;;; well, in Lecture 2b, anyway, we looked at solving "long-hand":
;; **

;; @@
(ns sicp.lecture.3b)




(require 'sicp.lecture.3b) ;  this is the trick to using the REPL for evaluation


;
; this is enough to do some simple symbolic derivatives
;   assuming we define (atomic? ...), (eq? ...), (const? ...)
;      and (same-var? ...)

; clojure specific
(= (first '(+ 1 2 3)) (symbol "+"))

; make-sum is not decalred yet...
(comment
  (eval (make-sum 3 5))
  (eval (make-prod 3 5)))

;
; but, like many first attempts, this produces very bad answers
; not wrong, but bad:
;
(def foo '(+ (* a (* x x)                 ; a(x^2) + b(x) + c
              (+ (* b x))
              c)))

(comment
  (deriv foo 'x))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Using rules from above:
;;; 
;;; ``` scheme
;;; ; 2ax + b
;;; 
;;; (+                                         ; rule #3 (u+v)
;;;  (+ (* a (+ (* x 1) (* x 1)))
;;;     (* 0 (* x x)))                         ; rule #5,  (uv)
;;;  (+ (* (* b 1) (* 0 x))                    ; rule #4,  (cu)
;;;     0))                                    ; rule #1,  (c)```
;;; 
;;; This is correct, but very messy
;; **

;; **
;;; ## Part 1
;;; 
;;; Robust systems need to be insensitive to changes in the problem
;;; 
;;; Small changes in the problem should only require small changes in the solution. The space of solutions shouild be continuous, just like the space of problems.
;;; 
;;; Don't develop a collection of solutions to specific problems, solve a whole colleciton of similar problems at once, which includes the one you really want
;;; 
;;; 
;;; 
;;; ```scheme
;;; ; numerical approximation of the derivative
;;; 
;;; 
;;; (defn deriv [f]
;;;   (fn [x]
;;;     (/ (- (f (+ x dx))
;;;           (f x))
;;;        dx)))
;;; 
;;; ```
;;; 
;;; but we don't want to confuse our thinking with solving the derivitive of some specific equations ith some specifc set of values, instead manipulating symbolic expressions and then plug in the values. 
;;; 
;;; Back to Calc 101:
;;; 
;;; 
;;; | Pattern    | Substitution        |
;;; |:----------:|:-------------------:|
;;; | dc/dx      | 0                   |
;;; | dx/dx      | 1                   |
;;; | d(u+v)/dx  | du/dx + dv/dx       |
;;; | dcu/dx     | c(du/dx)            |
;;; | duv/dx     | u(dv/dx) + v(du/dx) |
;;; 
;;; 
;;; 
;;; So, assuming we have a representation of the parts we need... some "wishful thinking"
;;; 
;;; 
;;; > Note: this is not the derivative of a function, but a maniplulation of the actual textual expression 
;;; 
;;; > Note: the extensive use of "wishful thinking"
;;; 
;;; 
;;; 
;;; 
;; **

;; **
;;; ####Wishful Thinking
;; **

;; @@
(defn atomic? [x]
  (cond (or (number? x)
            (symbol? x)
            (= (class x) clojure.lang.Symbol)) true
        :default false))
(defn eq? [e v] 
  (= e v))

(defn constant? [expr variable]
  (and (atomic? expr)
       (not (eq? expr variable))))

(defn same-var? [expr variable]
  (and (atomic? expr)
       (eq? expr variable)))

(defn sum? [expr]
  (and (not (atomic? expr))
       (= (first expr) (symbol "+"))))

(defn prod? [expr]
  (and (not (atomic? expr))
       (= (first expr) (symbol "*"))))

(atomic? '(+ a b))
(first '(+ a b))
(symbol "+")
(= (first '(+ a b)) (symbol "+"))
(sum? '(+ a b))



(defn make-sum [a1 a2]
  (list '+ a1 a2))

(defn make-prod [a1 a2]
  (list '* a1 a2))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3b/make-prod</span>","value":"#'sicp.lecture.3b/make-prod"}
;; <=

;; @@
(same-var? 'x 'y)
(make-sum 'x 'y)
(make-prod 'x 'y)

(eval (make-sum 1 2))
(eval (make-prod 1 2))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; **
;;; We need to have some representation of the symbolic function that we can pass into the deriv function:
;;; 
;;; Let's use "lists"
;;; 
;;; ```
;;; '(+ (* a (* x x))
;;;     (+ (* b x))
;;;     c))
;;;     
;;; ```
;;; 
;;; "car" (or "first") is the operator, "cdr" ("rest") holds the operands, recursively defined, as in Lisp
;;; 
;;; 
;; **

;; @@
; we need some helpers

(defn fun [x] (nth x 0)) ; the function is the first element

(defn a1 [x] (nth x 1))  ; a1 is the 2nd element
(defn a2 [x] (nth x 2))  ; a2 is the 3rd element

(defn m1 [x] (nth x 1))  ; m1 is the 2nd element
(defn m2 [x] (nth x 2))  ; m2 is the 3rd element
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3b/m2</span>","value":"#'sicp.lecture.3b/m2"}
;; <=

;; @@
(defn deriv [expr variable]
    (cond (constant? expr variable) 0
          (same-var? expr variable) 1
          (sum? expr) (make-sum (deriv (a1 expr) variable)
                                (deriv (a2 expr) variable))
          (prod? expr) (make-sum (make-prod (m1 expr) (deriv (m2 expr) variable))
                                 (make-prod (deriv (m1 expr) variable) (m2 expr)))))

(comment
  (def s__ '(+ a b))
  (def p__ '(* x y))


  (deriv 'c 'c)

  (deriv 'c 'x)

  (deriv 'x 'c)

  (deriv 'y 'c)


  (deriv (a1 s__) 'c)
  (deriv (a2 s__) 'c)

  (make-sum (deriv (a1 s__) 'c) (deriv (a2 s__) 'c))

  (make-prod (m1 p__) (deriv (m2 p__) 'c))

  (make-prod (deriv (m1 p__) 'c) (m2 p__))

  (make-sum (make-prod (m1 p__) (deriv (m2 p__) 'c))
            (make-prod (deriv (m1 p__) 'c) (m2 p__))))

(def l '(+ a b))
(sum? l)
(sum? '(+ a b))
(deriv '(+ a b) 'x)
(deriv '(+ (+ a b) (* x y)) 'x)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(+ 0 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(* x 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-symbol'>y</span>","value":"y"}],"value":"(* 1 y)"}],"value":"(+ (* x 0) (* 1 y))"}],"value":"(+ (+ 0 0) (+ (* x 0) (* 1 y)))"}
;; <=

;; @@
(deriv '(+ x x) 'x)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(+ 1 1)"}
;; <=

;; @@
(def foo '(+ (* a (* x x))
             (+ (* b x)
                c)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3b/foo</span>","value":"#'sicp.lecture.3b/foo"}
;; <=

;; @@
(deriv foo 'x)
(deriv foo 'a)
(deriv foo 'b)
(deriv foo 'c)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>a</span>","value":"a"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(* x 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 0 x)"}],"value":"(+ (* x 0) (* 0 x))"}],"value":"(* a (+ (* x 0) (* 0 x)))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* x x)"}],"value":"(* 0 (* x x))"}],"value":"(+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x)))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>b</span>","value":"b"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(* b 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 0 x)"}],"value":"(+ (* b 0) (* 0 x))"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(+ (+ (* b 0) (* 0 x)) 1)"}],"value":"(+ (+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x))) (+ (+ (* b 0) (* 0 x)) 1))"}
;; <=

;; **
;;; ## Part 2
;;; 
;;; Let's fix this messiness:
;;; 
;;; 
;;; the rules as defined in Clojure are "locally applied," but simplification (like optimization) often needs more "global" knowledge
;;; 
;;; ```
;;; (deriv foo 'a)
;;;    => (+                               ; x^2
;;;         (+ (* 0 (+ (* x 0) (* x 0)))
;;;            (* 1 (* x x)))
;;;         (+ (* (* b 0) (* 0 x))
;;;         0))
;;; 
;;; (deriv foo 'b)
;;;    => (+                               ; x
;;;         (+ (* 0 (+ (* x 0) (* x 0)))
;;;            (* 0 (* x x)))
;;;         (+ (* (* b 0) (* 1 x))
;;;         0))
;;; 
;;; (deriv foo 'c)
;;;    => (+                               ; 1
;;;         (+ (* 0 (+ (* x 0) (* x 0)))
;;;            (* 0 (* x x)))
;;;         (+ (* (* b 0) (* 0 x))
;;;         1))
;;; ```
;;; 
;;; notice the similar shapes, only the constants change with respect to the variable we take the derivative of
;;; 
;;; the rules are correct, just local and lacking a more global perspective needed for simplification (much lik the rational arithmetic we saw in lecture 2A)
;;; 
;;; we could solve the problem the same was as when we simplified the rational math in Lecture 2A - change the data representation
;;; 
;;; 
;;; 
;;; ###Simplification
;;; 
;;; We can rewrite `make-sum` to do simplification as part of making the "sum" derivative
;; **

;; @@
(defn make-sum [a1 a2]
  (cond (and (number? a1) (number? a2)) (+ a1 a2)
        (and (number? a1) (= a1 0)) a2
        (and (number? a2) (= a2 0)) a1
        :default (list '+ a1 a2)))

(make-sum 1 2)
(make-sum 'X 1)
(make-sum '2 'Y)
(make-sum 'X 'Y)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-symbol'>X</span>","value":"X"},{"type":"html","content":"<span class='clj-symbol'>Y</span>","value":"Y"}],"value":"(+ X Y)"}
;; <=

;; **
;;; A key thing to remember about simplifying `make-prod` is that there is a simplification for either parameter being 0, and a different simplification for when either parameter is 1.
;; **

;; @@
(defn make-prod [m1 m2]
  (cond (and (number? m1) (number? m2)) (* m1 m2)
        (and (number? m1) (= m1 1)) m2
        (and (number? m2) (= m2 1)) m1
        (and (number? m1) (= m1 0)) 0
        (and (number? m2) (= m2 0)) 0
        :default (list '* m1 m2)))

(make-prod 1 2)
(make-prod 'Y 0)
(make-prod 2 'X)
(make-prod 'X 'Y)
(make-prod 1 'y)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-symbol'>y</span>","value":"y"}
;; <=

;; **
;;; So now we can try the `deriv` function again with the new simplificaiton:
;; **

;; @@
(deriv foo 'x)
(deriv foo 'a)
(deriv foo 'b)
(deriv foo 'c)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; **
;;; So, for 
;;; ```
;;; foo = ax^2 + bx _c...```
;;; 
;;; we expect :
;;; 
;;; 
;;; | d(foo)/d__  | Substitution  |
;;; |:-----------:|:-------------:|
;;; |      x      | 2ax + b       |
;;; |      a      | x^2           |
;;; |      b      | x             |
;;; |      c      | 1             |
;;; 
;;; 
;; **

;; **
;;; Any more simplification and this starts to get very complicated (with lots of special cases).
;;; 
;;; Also, since we've _chosen_ the syntax for our symbolic expressions to be the *same* as our Lisp syntax, we need Lisp to have the (quote ...) syntax, so we can talk about Lisp expressions as data rather than evaluating them.
;; **
