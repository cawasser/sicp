;; gorilla-repl.fileformat = 1

;; **
;;; [Lambda Calculus Blog, Part 1](https://dzone.com/articles/lambda-calculus-in-clojure-part-1?utm_medium=feed&utm_source=feedpress.me&utm_campaign=Feed:%20dzone%2Fjava)
;; **

;; **
;;; This is an inline formula, @@\sin(x)@@, and this is on its own line:
;;; $$\int_0^{2\pi}\sin^2(x) \textrm{d}x$$ (but it might only work if this page is "online" so we can access the LaTEX parser - which doesn't work from home)
;; **

;; @@
(ns lambda
  (:gen-class))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Lambda Calculus
;;; 
;;; 
;;; We need a macro to wrap the "lambda" notation
;; **

;; @@
(defmacro λ
  [args & body]
  `(fn [~args] ~@body))




;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda/λ</span>","value":"#'lambda/λ"}
;; <=

;; **
;;; 
;; **

;; **
;;; ### Boolean Values
;;; 
;;; Let's start for some booleans, namely TRUE and FALSE. TRUE first.
;;; 
;;; > `λ.aλb.a` 
;;; 
;;; This is a function that takes two parameters and returns the first
;; **

;; @@
(def T 
  (λ a (λ b a)))

(def _T
  (fn [a]
    (fn [b]
      a)))

T

(T 1)

(T 0)

((T 1) 1)
((T 1) 0)

((T 0) 0)
((T 0) 1)

((T 5) 1)
((T 5) 0)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}
;; <=

;; @@
_T

(_T 1)

(_T 0)

((_T 1) 1)
((_T 1) 0)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; @@
((_T 0) 0)
((_T 0) 1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}
;; <=

;; **
;;; according to the DZone article, the idea is for TRUE, the function returns the first argument (the inner one) while FALSE will return the second (the outer one), which is what the Church Notation says.
;; **

;; @@
((T 'true) 'anything)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; Now for FALSE:
;;; 
;;; > `λ.aλb.b` 
;;; 
;;; Again we have a function that takes two parameters, but now returns the second
;; **

;; @@
(def F 
  (λ a (λ b b)))

(def _F
  (fn [a]
    (fn [b]
      b)))

((F 1) 0)
((F 0) 1)
((F 'anything) 'false)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; **
;;; Okay, that makes some sense, at least programatically. As far as actual use?
;;; 
;;; Getting used to the Church-style notiation is trick - the lambdas are all anonymous, so the identifiers are alway parameters, and everything is effectively "[Curried]()"
;;; 
;;; 
;;; ###Boolean Operations
;;; 
;;; We can define AND and OR in the same way...
;;; 
;;; 
;;; AND = `λp.λq.p q p`
;;; 
;;; Here we have a function of two paramters, `p` and `q` which returns `q` if `p` = TRUE or `p` when `p` is FALSE. 
;;; 
;;; > this is a round about way of saying that both `p` and `q` must be TRUE for AND to be TRUE
;; **

;; @@
(def And
  (λ p (λ q ((p q) p))))

(def _and
  (fn [p]
    (fn [q]
      ((p q) p))))


((And _T) _T)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/-T]</span>","value":"#function[lambda/-T]"}
;; <=

;; @@
((And _T) _F)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/-F]</span>","value":"#function[lambda/-F]"}
;; <=

;; @@
((And _F) _T)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/-F]</span>","value":"#function[lambda/-F]"}
;; <=

;; @@
((And _F) _F)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/-F]</span>","value":"#function[lambda/-F]"}
;; <=

;; **
;;; Now we can make "one" as a function that takes a function and returns the result of applying the outer function function  
;; **

;; @@
(def one
  (λ f (λ x (f x))))

(def succ
  (λ n (λ f (λ x (f ((n f) x))))))

(def plus
  (λ m (λ n ((n succ) m))))

(succ 1)

(plus 1)


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/plus$fn--16861]</span>","value":"#function[lambda/plus$fn--16861]"}
;; <=

;; **
;;; 
;; **

;; @@




;; @@

;; **
;;; now we can express various lambda function:
;;; 
;;; like "0", which we encode as a function that takes one parameter and return a function that takes one parameter.
;;; 
;;; In "Church Notation" i might look like:
;;; 
;;; `λa.λb.a`
;;; 
;;; In other words, a function of no parameters that returns a function of no parameters
;;; 
;; **

;; @@
(def zero
  (λ f (λ x x)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda/zero</span>","value":"#'lambda/zero"}
;; <=

;; @@


(


(def Or
  (λ p (λ q ((p p) q))))

(def toBoolean  
  (λ f ((f true) false)))

(toBoolean T)
(toBoolean F)


(toBoolean ((And T) T))
(toBoolean ((And F) T))
((Or T) F)
((Or F) F)

(def Not
  (λ p ((p F) T)))

(def Xor
  (λ a (λ b ((a (Not b)) b))))

(toBoolean (Not T))
(toBoolean (Not F))
(toBoolean ((Xor T) T))
(toBoolean ((Xor F) T))
((Xor T) F)
((Xor F) F)



(def If  
  (λ p (λ a (λ b ((p a) b)))))

(((If (Not F)) T) F)
(((If ((And T) T)) F) T)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[lambda/F]</span>","value":"#function[lambda/F]"}
;; <=

;; @@

;; @@
