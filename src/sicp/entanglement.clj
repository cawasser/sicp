;; gorilla-repl.fileformat = 1

;; **
;;; #Entanglement
;;; 
;;; Some experimental thinking on Quantum Entanglement, "simulated" using Lisp functions
;;; 
;; **

;; @@
(ns sicp.entanglement)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## 1. Schrodinger's Atom
;;; 
;;; In Clojure we have Atoms that provide an opaque container for some stateful data structure, much like Schrodinger's box [(link)](https://en.wikipedia.org/wiki/Schrödinger%27s_cat).
;; **

;; @@
(defn box []
  (atom (rand-int 2)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/box</span>","value":"#'sicp.entanglement/box"}
;; <=

;; @@
(box)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x6e1864d8 {:status :ready, :val 1}]</span>","value":"#object[clojure.lang.Atom 0x6e1864d8 {:status :ready, :val 1}]"}
;; <=

;; @@
@(box)
@(box)
@(box)
@(box)
@(box)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}
;; <=

;; **
;;; >NOTE: Gorilla REPL doesn't show all the results if they are duplicates
;;; 
;;; 
;;; ##1a. Schrodinger's Entanglement
;;; 
;;; Now we can have the box contain 2 data items (say, photons), whose value is dependent upon the other.
;;; 
;; **

;; @@
(defn schrodinger-entanglement []
  (if (= (rand-int 2) 0)
    (atom [0 1])
    (atom [1 0])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/schrodinger-entanglement</span>","value":"#'sicp.entanglement/schrodinger-entanglement"}
;; <=

;; **
;;; Here we return an atom that wraps a pair of entagnled values. By "entangled" here, I mean that the values depend upon each other. Once you know the value of one, you can deduce (know) the value of the other.
;; **

;; @@
(schrodinger-entanglement)
(schrodinger-entanglement)
(schrodinger-entanglement)
(schrodinger-entanglement)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x112cb31c {:status :ready, :val [0 1]}]</span>","value":"#object[clojure.lang.Atom 0x112cb31c {:status :ready, :val [0 1]}]"}
;; <=

;; **
;;; Now, technically, we can't see inside the box (atom) although Gorilla REPL actually shows us the values.
;; **

;; @@
(if (= (first @(schrodinger-entanglement)) 0)
  (str "first = 0, other = 1")
  (str "first = 1, other = 0"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;first = 0, other = 1&quot;</span>","value":"\"first = 0, other = 1\""}
;; <=

;; **
;;; #2. Separation
;;; 
;;; Physicists are doing experiments where they "physically" separate the paired photons. To simulate this, we probably need to move our "entangled photons" out of the same atom.
;; **

;; @@
(defn photons []
  (if (= (rand-int 2) 0)
    [(atom 0) (atom 1)]
    [(atom 1) (atom 0)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/photons</span>","value":"#'sicp.entanglement/photons"}
;; <=

;; @@
(photons)
(photons)
(photons)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x3e5d0986 {:status :ready, :val 1}]</span>","value":"#object[clojure.lang.Atom 0x3e5d0986 {:status :ready, :val 1}]"},{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x1d392cdf {:status :ready, :val 0}]</span>","value":"#object[clojure.lang.Atom 0x1d392cdf {:status :ready, :val 0}]"}],"value":"[#object[clojure.lang.Atom 0x3e5d0986 {:status :ready, :val 1}] #object[clojure.lang.Atom 0x1d392cdf {:status :ready, :val 0}]]"}
;; <=

;; @@
(def ph (photons))
(def a (first ph))
(def b (second ph))

ph
a
b

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x5eefce11 {:status :ready, :val 0}]</span>","value":"#object[clojure.lang.Atom 0x5eefce11 {:status :ready, :val 0}]"}
;; <=

;; **
;;; Now we can examine "a" directly and deduce the value of "b":
;; **

;; @@
(if (= @a 0)
  (str "b = 1")
  (str "b = 0"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""}
;; <=

;; **
;;; doing it a few times:
;; **

;; @@
(def deduce
  (if (= @(first (photons)) 0)
    (str "b = 1")
    (str "b = 0")))

deduce
deduce
deduce
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""}
;; <=

;; **
;;; Or we can make deduce a function:
;; **

;; @@
(defn deduce-fn [ph]
  (let [a @(first ph)]
    (if (= a 0)
      (str "b = 1")
      (str "b = 0"))))

(deduce-fn (photons))
(deduce-fn (photons))
(deduce-fn (photons))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""}
;; <=

;; **
;;; > Note: the evaluated values could change each time you evaluate this block, since we re looking at a differnt pair of photons each time (and they entangle via (rand-int ...))
;; **

;; **
;;; ##3. Encoding
;;; 
;;; Physicists talk of "encoding" data onto these entangled photons.
;;; 
;;; In Clojure, we probably need to change our photons from data values to functions themselves. Hmmm...
;;; 
;;; But what kinds of values are they encoding, and what kinds of properties are they expecting to observer?
;;; 
;;; ### 3a. Single Shared Value
;;; 
;;; This case is pretty simple, we produce a pair of entangled photons which have the *same* value encoded on/in them.  
;;; 
;; **

;; @@
(defn photons-3 [v]
  "v is the value we will encide onto the photons"
  (if (= (rand-int 2) 0)
    [(atom [0 v]) (atom [1 v])]
    [(atom [1 v]) (atom [0 v])]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/photons-3</span>","value":"#'sicp.entanglement/photons-3"}
;; <=

;; **
;;; Here we return a pair of photons, each of which actually has 2 values: the "entangled value" and the "encoded value."
;; **

;; @@
(photons-3 5)
(photons-3 50)
(photons-3 0.5)
(photons-3 "value")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x6a0b8636 {:status :ready, :val [0 &quot;value&quot;]}]</span>","value":"#object[clojure.lang.Atom 0x6a0b8636 {:status :ready, :val [0 \"value\"]}]"},{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x55823885 {:status :ready, :val [1 &quot;value&quot;]}]</span>","value":"#object[clojure.lang.Atom 0x55823885 {:status :ready, :val [1 \"value\"]}]"}],"value":"[#object[clojure.lang.Atom 0x6a0b8636 {:status :ready, :val [0 \"value\"]}] #object[clojure.lang.Atom 0x55823885 {:status :ready, :val [1 \"value\"]}]]"}
;; <=

;; **
;;; Now we need to reimplement (deduce ...) so we can learn both the entangled value and the encoded value by only looking a photon "a":
;;; 
;; **

;; @@
(def ph (photons-3 "value"))

(defn deduce-entangled [ph]
  (let [a @(first ph)]
    (if (= (first a) 0)
      (str "b = 1")
      (str "b = 0"))))


(defn deduce-encoded [ph]
  (let [a @(first ph)
        v (second a)]
    (str "b = " v)))




(deduce-entangled ph)

(deduce-encoded ph)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = value&quot;</span>","value":"\"b = value\""}
;; <=

;; **
;;; ###3b. Matched Values
;;; 
;;; In the last case we encoded the same value onto each photon, which isn't very useful. Now, let's have a pair of values, one of which is encoded on "a" and the other on "b." Again, once we know that value on "a" we can deduce the value on "b". 
;;; 
;;; >NOTE: we need to change the definition of ```(deduce-encoded ...)``` to actually look at the value attached to "a" and then provide the other value. So how do we know what the paired values are? We pass them to ```deduce```!
;; **

;; @@
(defn photons-3b [[v1 v2]]
  (if (= (rand-int 2) 0)
    [(atom [0 v1]) (atom [1 v2])]
    [(atom [1 v2]) (atom [0 v1])]))


(def values ["start" "end"])
(def ph3b (photons-3b values))

(defn deduce-encoded-3b [ph v]
  (let [a   @(first ph)
        idx (if (= (first a) 0) 1 0)]
    (str "b = " (nth v idx))))

ph3b
(deduce-entangled ph3b)
(deduce-encoded-3b ph3b values)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = start&quot;</span>","value":"\"b = start\""}
;; <=

;; **
;;; #### Question: Does it make any sense to encode the values randomly?
;;; 
;;; In such a case, we would assign both the entangled "photon" values and the "encoded" values randomly, and not pair-wise, as in example 3b, so we need to look at the "encoded value" of ```a```  and not just its "entangled value"
;; **

;; @@
(require '[clojure.core.match :refer [match]])


(defn photons-3b1 [[v1 v2]]
  (let [entangled (rand-int 2)
        encoded   (rand-int 2)]
    (match [entangled encoded]
           [0 0] [(atom [0 v1]) (atom [1 v2])]
           [0 1] [(atom [0 v2]) (atom [1 v1])]
           [1 0] [(atom [1 v1]) (atom [0 v2])]
           [1 1] [(atom [1 v2]) (atom [0 v1])])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/photons-3b1</span>","value":"#'sicp.entanglement/photons-3b1"}
;; <=

;; @@
(def ph3b1 (photons-3b1 ["first" "second"]))
ph3b1
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x6dfc13c1 {:status :ready, :val [0 &quot;second&quot;]}]</span>","value":"#object[clojure.lang.Atom 0x6dfc13c1 {:status :ready, :val [0 \"second\"]}]"},{"type":"html","content":"<span class='clj-atom'>#object[clojure.lang.Atom 0x6878e15a {:status :ready, :val [1 &quot;first&quot;]}]</span>","value":"#object[clojure.lang.Atom 0x6878e15a {:status :ready, :val [1 \"first\"]}]"}],"value":"[#object[clojure.lang.Atom 0x6dfc13c1 {:status :ready, :val [0 \"second\"]}] #object[clojure.lang.Atom 0x6878e15a {:status :ready, :val [1 \"first\"]}]]"}
;; <=

;; **
;;; If you run this enough times you'll eventually see a ```
;;; [[0 "second"] [1 "first"]]
;;; ``` 
;;; pairing which shows that the 2 values are independent.
;;; 
;;; So now ```(deduce-encoded ...)``` can't just use the entangled value of the photon to figure out the encoded value, it must look at the encoded value and deduce the opposite value:
;; **

;; @@
(defn deduce-3b1 [ph vs]
  (let [a @(first ph)]
    (str "b = "
         (if (= (second a) (first vs))
           (second vs)
           (first vs)))))

ph3b1
(deduce-3b1 ph3b1 ["first" "second"])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = first&quot;</span>","value":"\"b = first\""}
;; <=

;; **
;;; #4. Collapsing the Wave Function
;;; 
;;; So far, we've done all the creation of the entangled values immediately, right when the creation functions were run, but this is not the only possibility. We could always defer the computation until the value is actually needed.
;;; 
;;; This lets us explore what is sometimes called the [Copenhagen Interpretation](https://en.wikipedia.org/wiki/Copenhagen_interpretation), where the act of measurement triggers the collapse. As discussed on the Wikipedia page, [Niels Bohr](https://en.wikipedia.org/wiki/Niels_Bohr) had an alternative interpretation that involved the collapse taking place "whenever" and just "observed" by the human scientist at some time after that collapse.
;;; 
;;; 
;;; In our examples we take advantage of [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency) to hide the actual means of producing the value of the "photon"
;;; 
;; **

;; @@
(defn photons-4 []
  (fn []
    (if (= (rand-int 2) 0)
      [(atom 0) (atom 1)]
      [(atom 1) (atom 0)])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.entanglement/photons-4</span>","value":"#'sicp.entanglement/photons-4"}
;; <=

;; @@
(photons-4)
((photons-4))
(def e ((photons-4)))
e
(deduce-fn e)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""}
;; <=

;; @@
(deduce-fn ((photons-4)))
(deduce-fn ((photons-4)))
(deduce-fn ((photons-4)))
(deduce-fn ((photons-4)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""}
;; <=

;; **
;;; Notice how the photons-2 function doesn't return atoms, but a function that will return an atom. We could call this a ```Promise``` as implemented many languagues, including Clojure (via Java-Interop), but we are using a plain old Lisp function. 
;;; 
;;; 
;;; Powerful those Lisp function are... 
;;; 
;;; 
;;; ## 4a. Fun with ```photons-4```
;;; 
;;; We could put a bunch of photons into a map and play with them later
;; **

;; @@
(def some-photons (into [] (take 5 (repeatedly photons-4))))

some-photons

(nth some-photons 2)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[sicp.entanglement/photons-4$fn--15803]</span>","value":"#function[sicp.entanglement/photons-4$fn--15803]"}
;; <=

;; **
;;; we can look at all the "b" values of the photons, either by 'iteration' (list comprehension)
;; **

;; @@
(for [x (range 5)]
  (-> some-photons
      (nth,, x)
      (#(%))
      (deduce-fn)))

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""}],"value":"(\"b = 1\" \"b = 0\" \"b = 1\" \"b = 0\" \"b = 0\")"}
;; <=

;; **
;;; or by mapping
;; **

;; @@
(doall
  (map (fn [x] (-> x
                   (#(%))
                   deduce-fn))
       some-photons))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""},{"type":"html","content":"<span class='clj-string'>&quot;b = 0&quot;</span>","value":"\"b = 0\""}],"value":"(\"b = 1\" \"b = 0\" \"b = 0\" \"b = 0\" \"b = 0\")"}
;; <=

;; **
;;; >NOTE: We are not mapping over a collection of values, but the a collection of *functions*, so the answers are not the same because we are delaying the collapse of the wave function until we call ```(deduce-fn ...)``` . In a way, this could be more realistic, as we can get different answers depending upon _when_ we ask the question!
;;; 
;;; 
;;; ## 4.1 Scrodinger's Atom, Revisited
;;; 
;;; If we go back and revisit Schrodinger's Atom, we get implement the same behavior
;; **

;; @@
(defn box-4 []
  (fn []
    (atom (rand-int 2))))                                   ; define the box

(box-4)                                                     ; get back a function

((box-4))                                                   ; get back a photon (atom) from the function

@((box-4))                                                  ; get back the value from inside the photon


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; **
;;; but if we hold onto the box, we can look mutliple times and potentially get different values! It's as if the wave function collapses when we look and then re-forms when we stop looking so that it collapses again, to a possibly different value, the next time we look.
;; **

;; @@
(def b (box-4))

b
(b)

@(b)
@(b)
@(b)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"}
;; <=

;; **
;;; But this is *NOT* what we might expect...
;;; 
;;; ##4b. Schrodinger's Atom, Revised
;;; 
;;; What we expect to happen (perhaps?*) is that we collapse the wave function the first time, and then we get the same answer every time after that.
;;; 
;;; > *Maybe this _IS_ what we expect, that the wave function re-forms after we look, but I'm not convinced
;;; 
;;; 
;;; An easy way would be to add some "meta-data" to the photon, an extra value, that would tell the function if it has or hasn't been collapsed:
;; **

;; @@
(defn box-4b []
  (let [c  (atom false)
        ph (atom 0)]
    (fn []
      (if (not @c)                                          ; if we haven't yet collapsed the wave function...
        (do
          (reset! ph (rand-int 2))                          ; create the photon
          (reset! c true)                                   ; remember we HAVE collapsed the wave function
          ph)                                               ; and return it

        ph))))                                              ; if we already HAVE collapsed the wave function, the photon is already created


(def b (box-4b))
b

@(b)
@(b)
@(b)
@(b)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}
;; <=

;; **
;;; 
;;; 
;;; 
;;; 
;; **

;; **
;;; We can make the function return an entangled pair that remember they have been collapsed:
;; **

;; @@
(defn photons-4b []
  (let [c  (atom false)
        ph (atom [])]
    (fn []
      (if (not @c)
        (do
          (reset! c true)
          (reset! ph (if (= (rand-int 2) 0)
                       [(atom 0) (atom 1)]
                       [(atom 1) (atom 0)]))
          ph)
        ph))))


(def p (photons-4b))
p

@(p)
@(p)
@(p)
@(p)

(deduce-fn @(p))
(deduce-fn @(p))
(deduce-fn @(p))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = 1&quot;</span>","value":"\"b = 1\""}
;; <=

;; **
;;; And we can extend this to our encoded photons too:
;; **

;; @@
(defn photons-4b1 [[v1 v2]]
  (let [entangled (rand-int 2)
        encoded   (rand-int 2)
        c         (atom false)
        ph        (atom [])]
    (fn []
      (if (not @c)
        (do
          (reset! c true)
          (reset! ph (match [entangled encoded]
                            [0 0] [(atom [0 v1]) (atom [1 v2])]
                            [0 1] [(atom [0 v2]) (atom [1 v1])]
                            [1 0] [(atom [1 v1]) (atom [0 v2])]
                            [1 1] [(atom [1 v2]) (atom [0 v1])]))
          ph)
        ph))))

(def vals ["alpha" "beta"])
(photons-4b1 vals)

(def p4b1 (photons-4b1 vals))
p4b1

(deduce-3b1 @(p4b1) vals)
(deduce-3b1 @(p4b1) vals)
(deduce-3b1 @(p4b1) vals)
(deduce-3b1 @(p4b1) vals)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;b = alpha&quot;</span>","value":"\"b = alpha\""}
;; <=
