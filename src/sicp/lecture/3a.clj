;; gorilla-repl.fileformat = 1

;; **
;;; #Lecture 3A
;;; 
;;; SICP Lcture 3A examples
;;; 
;;; [Video](https://www.youtube.com/watch?v=2QgZVYI3tDs&list=PLB745DA2483BEE9C4&index=5)
;;; 
;;; 
;;; Lecturer: Abelson
;; **

;; @@
(ns sicp.lecture.3A
  (:require [sicp.lecture.2B :as s2b]))
            ;[quil.middleware :as m]
            ;[quil.core :as q]


(require 'sicp.lecture.3A)   ;  this is the trick to using the REPL for evaluation


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ##Part 1
;;; 
;;; More vector stuff
;; **

;; @@
(defn +vect [v1 v2]
  (s2b/make-vector
    (+ (s2b/x-coord v1) (s2b/x-coord v2))
    (+ (s2b/y-coord v1) (s2b/y-coord v2))))

(defn scale [v s]
  (s2b/make-vector
    (* s (s2b/x-coord v))
    (* s (s2b/y-coord v))))

(def v1 (s2b/make-vector 1 5))
(def v2 (s2b/make-vector 3 7))

v1
v2

(+vect v1 v2)     ; [4 12]
(scale v1 5)      ; [5 25]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"}],"value":"[5 25]"}
;; <=

;; **
;;; Another way, using our Lambda expressions from Lecture 2B
;; **

;; @@
(def _make-vector s2b/_cons)
(def _x-coord s2b/_car)
(def _y-coord s2b/_cdr)

(defn show-cons [x]
  [(s2b/_car x) (s2b/_cdr x)])

(defn _+vect [v1 v2]
  (_make-vector
    (+ (_x-coord v1) (_x-coord v2))
    (+ (_y-coord v1) (_y-coord v2))))

(defn _scale [v s]
  (_make-vector
    (* s (_x-coord v))
    (* s (_y-coord v))))

(def v3 (_make-vector 2 3))
(def v4 (_make-vector 5 1))

v3
v4

(show-cons (_+vect v3 v4)) ; [7 4]
(show-cons (_scale v3 5))  ; [10 15]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"}],"value":"[10 15]"}
;; <=

;; **
;;; New we can revisit segments...
;; **

;; @@
(def _make-segment s2b/_cons)
(def _seg-start s2b/_car)
(def _seg-end s2b/_cdr)

(def s1 (_make-segment (_make-vector 2 3) (_make-vector 5 1)))
(show-cons (_seg-start s1))   ; [2 3]
(show-cons (_seg-end s1))     ; [5 1]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[5 1]"}
;; <=

;; **
;;; We can use this to see in detail just how Lisp could do cons, car and cdr...
;; **

;; @@
(def l (s2b/_cons 1 (s2b/_cons 2 (s2b/_cons 3 (s2b/_cons 4 nil)))))
(s2b/_car l)
(s2b/_cdr l)
(s2b/_car (s2b/_cdr l))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; **
;;; In Clojure we use "first " and "rest" instead of car and cdr...
;; **

;; @@
(def _1-to-4 (list 1 2 3 4))
(first _1-to-4)
(first (rest _1-to-4))
_1-to-4
(rest _1-to-4)
(rest (rest (rest (rest _1-to-4))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"}
;; <=

;; **
;;; In Clojure we need to use ```(empty?)``` as only ```(nil? nil) -> true```, the empty list is FALSE!
;; **

;; @@
(nil? '())
(nil? nil)
(empty? '())


(defn scale-list [l s]
  (if (empty? l)
    nil
    (cons (* (first l) s)
          (scale-list (rest l) s))))
(scale-list _1-to-4 5)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"}],"value":"(5 10 15 20)"}
;; <=

;; **
;;; Now we can write a function ```(_map ...)``` that maps a function over a collection
;; **

;; @@
(defn _map [f l]
  (if (empty? l)
    nil
    (cons (f (first l))
          (_map f (rest l)))))


(_map #(* % 5) _1-to-4)
(_map #(+ % 10) _1-to-4)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"}],"value":"(11 12 13 14)"}
;; <=

;; **
;;; Then we can use that function to write a scale function that scales a whole list of numbers ```(scale ...)```
;; **

;; @@
(defn _scale-to-list [l s]
  (_map #(* s %) l))


(_scale-to-list _1-to-4 5)
(_scale-to-list _1-to-4 25)
(_scale-to-list _1-to-4 0.5)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.5</span>","value":"0.5"},{"type":"html","content":"<span class='clj-double'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>2.0</span>","value":"2.0"}],"value":"(0.5 1.0 1.5 2.0)"}
;; <=

;; **
;;; Lisp is cool beacause you can do such powerful things with so little code, even from scratch (like making cons, car and cdr out of functions!)
;;; 
;;; This notion of performing functions over entire collections is also expressed in the APL language.
;;; 
;;; 
;;; We can also write a map-like function whose purpose if to perform side-effects (and **not** return a result)
;; **

;; @@
(defn for-each [f l]
  (if (empty? l) "done!"
      (doall
        (first l)
        (for-each f (rest l)))))



(for-each #(str %) _1-to-4)
(for-each #(str %) '(1 2 3 4 5))
(doall (map #(str %) _1-to-4))

(str (first _1-to-4))
(str "hello")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;hello&quot;</span>","value":"\"hello\""}
;; <=

;; **
;;; >Note: This code doesn't do the side-effects as expected (the input values are never printed). I think this is due to Clojure's laziness, but I'm not sure how to correct this.
;; **

;; **
;;; ##Part 2
;;; 
;;; Primitives
;;; 
;;; - Means of Combination (Composition)
;;; - Means of Abstraction (for some definition of "abstraction")
;;; 
;;; 
;;; Peter Henderson:
;;; 
;;; >recursive graphics of "stamped elements"
;;; - woodcuts, etc.
;;; - see [31:00]() in the video
;;; 
;;; |            |         |
;;; |:-----------|:-------|
;;; | 1 primitive | Picture |
;;; | combinations | rotate, flip, beside, above|
;;; 
;;; pictures are "closed" over all the combination functions (they all return another picture)
;; **

; TODO - get link to spot in video

;; **
;;; So let's build the underlying abstraction of a rectangle:
;;; 
;;; >- a picture will "draw" itself inside a rectangle
;; **

;; @@
(defn make-rect [orig w h]
  {:origin orig :w w :h h})
(defn horiz [r]
  (:w r))
(defn vert [r]
  (:h r))
(defn origin [r]
  (:origin r))
(defn origin-x [r]
  (_x-coord (:origin r)))
(defn origin-y [r]
  (_y-coord (:origin r)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/origin-y</span>","value":"#'sicp.lecture.3A/origin-y"}
;; <=

;; **
;;; Now we can start using the rectangle to build up our primative "type":
;; **

;; @@
; this fakes drawing to the screen for visualization
(defn drawline [from-p to-p])


(defn coord-map [r]
  (fn [p]
    (+vect (+vect (scale (_x-coord p) (horiz r))
                  (scale (_y-coord p) (vert r)))
           (origin r))))


(defn make-pict [segs] ; segs is expressed in a unit-rectangle
  (fn [r]                 ; return a function that takes a rect (which is a picture)
    (for-each #(drawline ((coord-map r) (_seg-start %))
                         ((coord-map r) (_seg-end %)))
              segs)))

; we use (for-each ...) to map our side-effecting (draw-line ...) across all the segments of the picture
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/make-pict</span>","value":"#'sicp.lecture.3A/make-pict"}
;; <=

;; **
;;; 
;; **

;; **
;;; notice how closures tie functions to data like an OO class, but without the downsides
;; **

;; @@
(defn setup []
  ; initial state
  {:segments []})

(defn draw-segment [s]
  (let [start (_seg-start s)
        end (_seg-end s)]
    (q/line (_x-coord start) (_y-coord start) (_x-coord end) (_y-coord end))))


(defn draw [state]
  (q/background 255)
  (for [s (:segments state)]
    (draw-segment s)))



(def r (make-rect [0 0] 10 10))

(def g (make-pict []))

(g r)         ; draw image "g" inside rectangle "r"
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;done!&quot;</span>","value":"\"done!\""}
;; <=

;; **
;;; ## Part 3
;;; 
;;; Now we can do interesting things, like perform trancformations and combinations of pictures. For example, we can "draw" 2 pistures side by side using a function called ```(beside picture-1 picture-2 a)``` where "a" is the relative horizontal split betewwen the left and right halves. 
;;; 
;;; | a  | split |
;;; |:-:|:-:|
;;; | 0.5 | 50/50|
;;; | 0.1 | 10/90 |
;;; | 0.6 | 60/40 |
;;; 
;; **

;; @@
(defn beside [p1 p2 a]
  (fn [r]               ; must return a function that takes a rect (the definition of a Picture)
    (p1 (make-rect
          (origin r)
          (scale a (horiz r))
          (vert r)))
    (p2 (make-rect
          (+vect (origin r)
                 (scale a (horiz r)))
          (scale (- 1 a) (horiz r))
          (vert r)))))

; remember, we don't need to return anything, because the lambda itself is the return values, a closure over the 2 pictures, returning 2 revised pictures
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/beside</span>","value":"#'sicp.lecture.3A/beside"}
;; <=

;; **
;;; We can rotate a pciture by 90*.
;;; 
;;; I one way of looking at this, we take a rectagle ([0 0] 5 10) and rotate it such that it now is ([5 0] 10 5).
;; **

;; @@
(defn rotate-90 [p]
  (fn [r]
    (p (make-rect (+vect (origin r)
                         (horiz r))
                  (vert r)
                  (scale -1 (horiz r))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/rotate-90</span>","value":"#'sicp.lecture.3A/rotate-90"}
;; <=

;; **
;;; Or we can combine a "beside" with a "push", meaning recursively set a smaller version of the picture beside itself.
;;; >see 59:50
;; **

;; @@
(defn right-push [p n a]
  (if (= n 0)
      p
      (beside p (right-push p (- n 1) a) a)))    ; these both p and this return a lambda of rect
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/right-push</span>","value":"#'sicp.lecture.3A/right-push"}
;; <=

;; **
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; note that including (beside ...) ties us to that approach. Can we make it more arbitrary?
;; **

;; @@
;
; a generalization of push
;
(defn push [comb]
  (fn [pict n a]
    ((take n
          (repeatedly
            (fn [p]
              (comb pict p a))))
     pict))) ; apply the whole repeated thing to the original picture

; so...

(def right-push (push beside)) ; this definition of right-push in an application of push into beside

;(def above-push (push above))
; etc...
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.3A/right-push</span>","value":"#'sicp.lecture.3A/right-push"}
;; <=

;; **
;;; Notice the similarities with the original ```(right-push ...)```. This is intentional, since we are simply trying to generalize the right-push implementation.
;;; 
;;; 
;;; #Summary
;;; 
;;; This approach, building languages on top of languages:
;;; 
;;;     (pictures -> geometries -> schemes and combinations)
;;; 
;;; is better than the top-down decomposition, the simpler way we might approach the problme space of doing symbolic derivatives, because it has full expressive power at each level.
;;; 
;;; In a top down approach, each node typically solves just 1 problem. When we solve at a "language level" we solve ALL problems that such a language can express, we just need enough means of combination (push, beside, above, below, etc.) to meet the complexity needs of the problem space.
;;; 
;;; Top-down tends to be brittle - any change affects everything above and below. Language-based solutions are flexible, becasue we have a real language to develop aditional means of combination. We can express present and future problems in such a language.
;;; 
;;; Ableson draws it like this:
;;; 
;;; ![alt text](src/keynote/jpgs/3a-1.jpg)
;;; 
;;; Each layer provides its own vocabulary for expressing changes within that layer.
;; **

;; @@
 
;; @@
