;; gorilla-repl.fileformat = 1

;; **
;;; #Lecture 2B
;;; 
;;; SICP Lecture 2B examples
;;; 
;;; [Video](https://www.youtube.com/watch?v=ymsbTVLbyN4&index=4&list=PLB745DA2483BEE9C4)
;;; 
;;; Lecturer: Abelson
;; **

;; @@
(ns sicp.lecture.2B
  (:gen-class
   :require [[clojure.core/math :refer :all]
             [sicp.lecture.2A :refer :all]]))

(require 'sicp.lecture.2B)   ;  this is the trick to using the REPL for evaluation

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; 
;;; ##DATA ABSTRACTION
;;; 
;;; 
;;; abstraction:
;;; 
;;; "divorcing the task or building things from the task of implementing the parts" - Abelson
;;; 
;;; I'd call this "encapsulation" or "hiding"
;;; 
;;; 
;;; 
;;;  one advantage is "naming" - having the power of the name
;;; 
;;;  this approach give us the power of having a "conceptual entity" (type?)
;;; 
;;; 
;;; 
;; **

;; **
;;; ###part 1 & 2
;;; 
;;; I'm using a Clojurey approach, just popping the terms into a vector
;;; 
;;; Abelson uses Lists, since he is using Scheme so: 
;;; 
;;; * (defn make-rat [x y] '(x y))
;;; 
;;; * (defn numer [x] (first x))     ; (car x)
;;; 
;;; * (defn demon [x] (second x))    ; (cdr x)```
;;; 
;;; 
;;; 
;; **

;; @@
(defn make-rat [x y]
  [x y])

(defn numer [x]
  (nth x 0))

(defn denom [x]
  (nth x 1))

(defn +rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn *rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(def a (make-rat 1 2))
(def b (make-rat 1 4))
(def c (make-rat 3 4))
(def d (make-rat 2 3))

(+rat a b)                    ; [6 8]
(*rat c d)                    ; [6 12]

;
; note that these are not reduced to common factors
;
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"}],"value":"[6 12]"}
;; <=

;; **
;;; use gcd if we have a "rat" [x], of just 2 number ([a b])
;; **

;; @@
(defn gcd
  ([a b]
   (if (zero? b)    
     a    
     (recur b (mod a b)))) 
 
  ([x]
   (let [n (nth x 0)
         d (nth x 1)]
     (gcd n d))))
   


(defn reduce-rat [x]
  (let [n (numer x)
        d (denom x)
        g (gcd x)]
    [(/ n g) (/ d g)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2B/reduce-rat</span>","value":"#'sicp.lecture.2B/reduce-rat"}
;; <=

;; **
;;; we could use destructuring : `(defn reduce [[n d]] ...)`, but then we are tied to this specific implementaton (vectors)
;; **

;; **
;;; 
;; **

;; @@

(gcd 6 8)                         ; 2
(gcd 6 12)                        ; 6
(reduce-rat (+rat a b))           ; [3 4]
(reduce-rat (*rat c d))           ; [1 2]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 2]"}
;; <=

;; **
;;; some extra stuff
;; **

;; @@
(defn eq-rat [a b]
  (let [red-a (reduce-rat a)
        red-b (reduce-rat b)]
    (and (= (numer red-a) (numer red-b))
         (= (denom red-a) (denom red-b)))))

(eq-rat (make-rat 1 2) (make-rat 4 8))         ; true
(eq-rat (make-rat 1 2) (make-rat 3 4))         ; false
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}
;; <=

;; **
;;; a different representation: do the reduce as part of numer and demon
;; **

;; **
;;; 
;; **

;; **
;;; 
;; **

;; @@
(defn numer [x]
  (let [n (nth x 0)
        g (gcd x)]
    (/ n g)))

(defn denom [x]
  (let [d (nth x 1)
        g (gcd x)]
    (/ d g)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2B/denom</span>","value":"#'sicp.lecture.2B/denom"}
;; <=

;; **
;;; this doesn't change the need to reduce after the computation, as we only do the gcd when we pull the "rat" apart
;; **

;; @@
a
b

(+rat a b)                    ; [6 8]
(numer (+rat a b))            ; 3
(denom (+rat a b))            ; 4

(*rat c d)                    ; [6 12]
(numer (*rat c d))            ; 1
(denom (*rat c d))            ; 2
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; **
;;; 
;;; the issue is: can we decide which implementation is the best one?
;;; 
;;; well... under some circumstanced, we would be better off NOT deciding
;;; we should defer the decision as long as possible
;;; 
;;; encapsulating the implentation behind the "name" give us this flexibility
;;; 
;;; 
;;;  > "... people who say you should design everything before you implement it 
;;;  basically are people who haven't designed very many things." - Abelson
;;; 
;;;  > "..the real power is that you can pretend to make the decision, and later on
;;;  figure out which decision you ought to have made." - Abelson
;;; 
;;; 
;; **

;; **
;;; ##part 3
;;; 
;;; data abstraction is about developing building-blocks we can build other things with
;;; 
;; **

;; **
;;; Start with a point, or vector (if we assume the vector "starts" at `(0,0)`)
;; **

;; @@
(defn make-vector [x y]
  [x y])

(defn x-coord [p]
  (nth p 0))

(defn y-coord [p]
  (nth p 1))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2B/y-coord</span>","value":"#'sicp.lecture.2B/y-coord"}
;; <=

;; **
;;; add line segments:
;; **

;; @@
(defn make-seg [p q]
  [p q])

(defn seg-start [s]
  (nth s 0))

(defn seg-end [s]
  (nth s 1))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2B/seg-end</span>","value":"#'sicp.lecture.2B/seg-end"}
;; <=

;; **
;;; now do things with segments:
;; **

;; @@

(require 'sicp.lecture.2A) ; so we can use some functions from 2A

(def p (make-vector 1 2))
(def q (make-vector 2 3))

(defn seg-midpoint [s]
  (let [a (seg-start s)
        b (seg-end s)]
    (make-vector (sicp.lecture.2A/my-avg (x-coord a) (x-coord b))
                 (sicp.lecture.2A/my-avg (y-coord a) (y-coord b)))))

p

q

(def s (make-seg p q))

s

(seg-midpoint s)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-ratio'>3/2</span>","value":"3/2"},{"type":"html","content":"<span class='clj-ratio'>5/2</span>","value":"5/2"}],"value":"[3/2 5/2]"}
;; <=

;; **
;;; now get the length of a segment via Pythagorean Theorem:
;; **

;; @@
(defn seg-length [s]
  (let [dx (- (x-coord (seg-end s)) (x-coord (seg-start s)))
        dy (- (y-coord (seg-end s)) (y-coord (seg-start s)))]
    (sicp.lecture.2A/sq-rt (+ (sicp.lecture.2A/sq dx) (sicp.lecture.2A/sq dy)))))

(seg-length s)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.414213562373095</span>","value":"1.414213562373095"}
;; <=

;; **
;;; the real question is: is this level of data hiding worth the effort?
;;; 1. how much leverage do we gain?   (little, in this case)
;;; 2. what are the chances we'll change the implementation? (microscopic)
;;; 
;;; Abelman discusses "closures" (this implementation could assemble pairs of anything) and then he discusses various ways the implementation might change
;;; - left to right drawing
;;; - top to bottom
;;; - etc
;; **

;; **
;;; ###part 4
;;; 
;;; 
;;; `make-rat`, `numer`, and `denom` are an API on the "abstract" data "vectors" and "segments"
;;; 
;;; 
;;; 
;;; Likewise, the contract for "Rational Numbers" is:
;;; 
;;; ```scheme
;;; IF x = (make-rat n d)
;;; THEN
;;;      (numer x)     n
;;;      ---------  = ---
;;;      (denom x)     d```
;;; 
;;; 
;;; This is really what a rational number is - this axiom (this contract)
;;; 
;;; > interestingly, this seems to fit with the Whitehead idea that reality (things) are the outcome of relationships (methods or functions)
;;; 
;;; AXIOM for "pairs":
;;; 
;;; ```
;;; FOR any x and y,
;;;        (car (cons x y)) -> x
;;;        (cdr (cons x y)) -> y```
;;; 
;;; in clojure
;;; ```clojure
;;;    (nth [x y] 0) -> x
;;;    (nth [x y] 1) -> y```
;;; 
;;; 
;;; 
;;; so we can make the pairs out of "air"!
;;; 
;; **

;; @@
(defn _cons [a b]
    (fn [pick]
      (cond (= pick 1) a
            (= pick 2) b)))

(defn _car [x] (x 1)) ; (_car) is a function that takes a function
(defn _cdr [x] (x 2)) ; as is (_cdr)

(def a (_cons 1 2))
(def b (_cons a 3))
(def c (_cons 1 2))

(_car a)
(_cdr a)
(_car b)
(_cdr b)
(_car (_car b))
(comment
  (_car (_cdr b))) ; <- this throws an error because '3' doesn't have a (_cdr)

(= a c)  ; these are 2 different instances of the _cons closure
(not (= a c))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; > Note: that there is no data here - the values are held in the parameters to the lambda functions returned by (_cons)
;;; 
;;; The _function __IS__ our data!_ because each call is a closure over the parameters, so the values are held by the runtime environment as part of the lambda closure.
;; **
