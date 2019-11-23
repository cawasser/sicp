;; gorilla-repl.fileformat = 1

;; **
;;; #Lecture 2A
;;; 
;;; SICP Lecture 2A examples
;;; 
;;; [Video](https://www.youtube.com/watch?v=erHp3r6PbJk&index=3&list=PLB745DA2483BEE9C4)
;;; 
;;; Lecturer: Sussman
;; **

;; @@
(ns sicp.lecture.2A
  (:gen-class
   :require [clojure.core/math :refer :all]))


(require 'sicp.lecture.2A)   ;  this is the trick to using the REPL for evaluation


(defn foo [x]
  (println x "hello World!"))



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2A/foo</span>","value":"#'sicp.lecture.2A/foo"}
;; <=

;; @@
; by-hand for comparison
;
(apply + (range 1 41))    ; (sum-ints 1 40)
(+ 1 4 9 16)              ; (sum-sq 1 4)
(+ 0.5 1 1.5 2)           ; (sum-stuff 1 10)

; util functions
;
(defn sq [a]
  (* a a))

(defn ident [a]
  a)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2A/ident</span>","value":"#'sicp.lecture.2A/ident"}
;; <=

;; **
;;; original recursive approach (each is independent and therefore, duplicative of the others
;; **

;; @@
(defn sum-ints [a b]
  (if (> a b) 0 (+ a (sum-ints (inc a) b))))
(sum-ints 1 40)        ; 820

(defn sum-sq [a b]
  (if (> a b) 0 (+ (sq a) (sum-sq (inc a) b))))
(sum-sq 1 4)           ; 30

(defn sum-stuff [a b]
  (if (> a b) 0 (+ (/ a 3) (sum-stuff (* a 2) b))))
(sum-stuff 1 10)       ; 5

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-bigint'>5N</span>","value":"5N"}
;; <=

;; **
;;; a recursive (shape) function built using recursion
;; **

;; @@
(defn summation [a b f nxt]
  (if (> a b)
    0
    (+ (f a) (summation (nxt a) b f nxt))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2A/summation</span>","value":"#'sicp.lecture.2A/summation"}
;; <=

;; **
;;; redefine in terms of (summation ...)
;; **

;; @@
(defn sum-ints [a b]
  (summation a b ident inc))
(sum-ints 1 40)        ; 820

(defn sum-sq [a b]
  (summation a b sq inc))
(sum-sq 1 4)           ; 30

(defn sum-stuff [a b]
   (summation a b #(/ % 3) #(* % 2)))
(sum-stuff 1 10)       ; 5
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-bigint'>5N</span>","value":"5N"}
;; <=

;; **
;;; using summation directly
;; **

;; @@
(summation 1 40 ident inc)          ; sum-ints     820
(summation 1 4 sq inc)              ; sum-sq        30
(summation 1 10 #(/ % 3) #(* % 2))  ; sum-stuff      5
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-bigint'>5N</span>","value":"5N"}
;; <=

;; **
;;; another crazy summation
;; **

;; @@
(defn pi-sum [a b]
  (summation a b 
    #(/ 1 (* % (+ % 2))) ; f
    #(+ % 4))) ; nxt

(pi-sum 1 12)         ; 1289/3465
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1289/3465</span>","value":"1289/3465"}
;; <=

;; **
;;; an iternative (shape) function built using recursion
;; **

;; @@
(defn sum-iter [a b f nxt]
  (defn- iter [j ans]
    (if (> j b)
      ans
      (iter (nxt j) (+ (f j) ans))))
  (iter a 0))

(sum-ints 1 4)                                 ;                 10
(sum-iter 1 4 ident inc)                       ; sum-ints        10
(sum-iter 1 40 sq inc)                         ; sum-sq       22140
(sum-iter 1 10 #(/ % 3) #(* % 2))              ; sum-stuff        5
(sum-iter 1 12 #(/ 1 (* % (+ % 2))) #(+ % 4))  ; pi-sum   1289/3465  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-ratio'>1289/3465</span>","value":"1289/3465"}
;; <=

;; **
;;; ####Personal Exploration
;;; 
;;; summation is similar to reduce, but with implicit range rather than explict
;;; * cmp - compose function
;;; * r   - identity value for the compose function (+/- = 0, * = 1, etc.)
;;; * a b - from to values
;;; * f   - function compute the value to 'compose' into the answer
;;; * nxt - function to get the next 'a' value
;;; 
;; **

;; @@
(defn my-reduce [cmp r a b f nxt]
  (if (> a b)
    r
    (cmp (f a) (my-reduce cmp r (nxt a) b f nxt))))
(my-reduce + 0 1 40 ident inc)                  ;   820
(my-reduce + 0 1 40 sq inc)                     ; 22140
(my-reduce + 0 1 10 #(/ % 3) #(* % 2))          ;     5
;
(my-reduce * 1 1 5 ident inc)                   ;   120
(reduce * [1 2 3 4 5])                          ;   120
;
(- 0 1)                                         ;    -1
(- -1 2)                                        ;    -3
(- -3 3)                                        ;    -6
(my-reduce - 0 1 1 ident inc)                   ;     1
(reduce - [1 2 3 4 5])                          ;    13
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>-13</span>","value":"-13"}
;; <=

;; **
;;; ###Heuron of Alexandria's method for computing Square Root
;; **

;; @@
(defn my-abs [x]
  (if (>= x 0)
    x
    (- x)))

(defn my-avg [x y]
  (/ (+ x y) 2))

(def tolerance 0.00001)

(defn sq-rt [x]
  (def tolerance 0.000000000001)
  
  (defn good-enf? [y x]
    (< (my-abs (- (* y y) x)) tolerance))
  
  (defn improve [y x]      ; the "fixed point" of this function
    (my-avg (/ x y) y))    ; is the square-root of x
  
  (defn my-try [y x]
    (if (good-enf? y x)
      y
      (my-try (improve y x) x)))
  
  (my-try 1. x))

(sq-rt 25)             ;   5.0
(sq-rt 2)              ;   1.414213562373095
(sq-rt 144)            ;  12.0
(sq-rt 13456)          ; 116.0
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>116.0</span>","value":"116.0"}
;; <=

;; **
;;; another approach using "Fixed-Point"
;;; 
;;; a fixed-point is a value 'x' for a function 'f' such that `f(x) -> x` (or vice versa)
;;; 
;;; > this works for sq-root because the improve function coverges onto the answer (f(x) -> x) which is recognized by the "close-enf?" function 
;; **

;; @@
(defn fixed-point [f start]
  (defn close-enf? [u v]
    (< (my-abs (- u v)) tolerance))
  
  (defn iter [old new]
    (if (close-enf? old new)
      new
      (iter new (f new))))
  
  (iter start (f start)))

(defn s-r [x]
  (fixed-point             
    #(my-avg (/ x %) %) ; this function is the fixed-point "finder" in Heuron's method
    1.))

(s-r 25)             ;   5.0
(s-r 2)              ;   1.414213562373095
(s-r 144)            ;  12.0
(s-r 13456)          ; 116.0
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>116.0</span>","value":"116.0"}
;; <=

;; **
;;; a simpler fixedpoint function for sq-root is `y -> x/y`
;;; 
;;; but this doesn't work because the function just oscillates
;;; 
;;; ;  (/ x y))
;; **

;; @@
(defn smpl-sqroot [x]
  (fixed-point #(/ x %) 1.))

(comment
  (smpl-sqroot 25))  ; <- never converges...

(defn avg-damp [f]
  (fn [x]
    (my-avg (f x) x)))

(defn damp-sqroot [x]
  (fixed-point (avg-damp #(/ x %)) 1.))

(damp-sqroot 25)             ;   5.0
(damp-sqroot 2)              ;   1.414213562373095
(damp-sqroot 144)            ;  12.0
(damp-sqroot 13456)          ; 116.0
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>116.0</span>","value":"116.0"}
;; <=

;; **
;;; newtons method to find the roots of functions (zeros)
;;; 
;;; 
;;; find `y` such that `f(y) -> 0`
;;; 
;;; > Note: see [45:00]() in the video
;; **

; TODO - get link to spot in video

;; @@
(def dx 0.0000001)

(defn deriv [f]
  (fn [x] 
    (/ (- (f (+ x dx))
          (f x)) 
       dx)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2A/deriv</span>","value":"#'sicp.lecture.2A/deriv"}
;; <=

;; **
;;; run Newton's method using fixed-point, using the function 'f' to determine what kind of value we are finding
;; **

;; @@
(defn newton [f start]
  (defn df [x] ((deriv f) x))
    
  (fixed-point (fn [x] (- x (/ (f x) (df x))))
    start))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.lecture.2A/newton</span>","value":"#'sicp.lecture.2A/newton"}
;; <=

;; **
;;; in our case, we are finding square-roots using:
;;; 
;;; `y -> x - y^2`
;;; 
;;; > _i.e., the Netwon function for finding square-roots_
;; **

;; @@
(defn newton-root [x]
  (newton (fn [y] (- x (sq y))) 
          1))

(newton-root 25)
(newton-root 144)
(newton-root 2)
(newton-root 13456)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>116.0</span>","value":"116.0"}
;; <=

;; **
;;; experiment to stick a function inside a clojure data-structure
;; **

;; @@
(def ds {:square sq :sq-root newton-root})

((:square ds) 5)                 ; yup! it works
((:sq-root ds) ((:square ds) 5)) ; ... and it again!
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"}
;; <=
