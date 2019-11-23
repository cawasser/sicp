;; gorilla-repl.fileformat = 1

;; **
;;; # Resource Management
;;; 
;;; 
;;; ## An Experiment in Designing a "Language" to Express A _Supply/Demand_ Solution
;;; 
;;; Supply and Demand are fundamental concepts in the discussion of "[markets](https://en.wikipedia.org/wiki/Supply_and_demand)" for the exchange of goods or services. A "good" is defined as a single unit of commodity (or service) for exchange, and "supply" can be thought of as the production of such goods, while "demand" is the desire for exchange to acquire some of those goods.
;;; 
;;; ### Key Concepts
;;; 
;;; - **Good**: the basic unit of "work" produced by one or more _Sellers_. A _Good_ could be a thing, like a car or a cellphone, or a service, like Internet Bandwidth or mowing a lawn. At its most generic, a good is nothing more than "something of value" that can be exchanged between parties (_Buyers_ and _Sellers_).
;;; 
;;; - **Buyer**: an entity that desires to exchange some _Currency_ for _Goods_. 
;;; 
;;; - **Currency**: in the most generic sense, a _Currency_ is simply unit specifying an agreed-upon denotation of the value allocated to a given _Good_. 
;;; >For example, when we say "The sandwich is worth $5", we mean that the _Good_ "sandwich" has a value denoted in the _Currency_ of "dollars" ($), as 5.
;;; 
;;; 
;;; >see also Richard Wolff's [_"Markets Don't Supply According to Demand"_]() - "markets supply what buyers can afford"
;;; 
;;; - **Seller**: an entitiy (person, organization, natural process, etc.) that "creates" a _Good_. In the real-world, suppliers are almost always themselves _Purchasers_ of _Goods_ that are then turned into the _Good_ they market. In some cases a _Seller_ is not, strictly, a supplier, in that they do not "create" the _Good_ but re-sell _Goods_ purchases elsewhere (see also, [Arbitrage](https://en.wikipedia.org/wiki/Arbitrage)). Within the confines of a given _Market_ this distinction typically makes no difference.
;;; 
;;; - **Supply**: the aggregate of _Goods_ on offer from all _Suppliers_ within a given _Market_.
;;; 
;;; - **Demand**: the aggregate of _Goods_ requires to satisfy all _Buyers_ within a given _Market_.
;;; 
;;; - **Bid**: the amount of _Currency_ a _Buyer_ is willing to exchange for an identified quantity of _Goods_. 
;;; > For example, "Bid is $50 for 100 units of Bandwidth".
;;; 
;;; - **Ask**: the amount of _Currency_ a _Seller_ is requiring for the exchange of an identified qualtity of _Goods_. 
;;; > For example, "Ask is $100 for 100 units of Bandwidth". 
;;; 
;;; - **Market**: sometimes called an _Exchange_, a _Market_ represents a place where _Buyers_ and _Seller_ meet to exchange _Goods_ for _Currency_.
;;; 
;;; 
;; **

;; @@
(ns sicp.resource-mgmt
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Defining a _Good_
;;; 
;;; Fundamentally, a _Good_ is nothing more than a "type" which can de used to denote what purpose the _Good_ serves.
;;; 
;;; > _I now think of this not as a 'type', but as a 'semantic' a name for something that has meaning in our "world." This is called on by the phrase "purpose the **Good** server" in the statement above._
;;; 
;;; > For example, 	
;;; 		- Bandwidth
;;; 		- Oranges
;;;     	- Electricity
;;;     	- Crude Oil
;;;     
;;; ### Properties 
;;; 
;;; Typically, _Goods_ have some number of well-defined _Properties_ that both help more uniquely identify them and help determine their _Ask_ and _Bid_ prices.
;;; 
;;; > For example, Bandwidth might have properties such as:
;;; 		- date rate
;;;         - error correction
;;;         - quality of service level
;;;         
;;; > or, Oranges might have:
;;; 		- variety
;;;         - date picked
;;;         - moisture content
;;;         
;;; ### Unit Of Sale
;;; 
;;; Since we probably don't want to be marketing or purchasing individual oranges on our _Exchange_, an important property of a _Good_ is its _Unit of Sale_. This is a measure of the quantity of individual items that make up a salable _Good_. For oranges it might be a "bushel" or perhaps "truckload", or even just "metric ton".
;;; 
;;; It is entirely possible for a given _Good_ to have multiple _Units of Measure_, along with rules for converting between them. For oranges we might say a "truckload" equals 2 "metric tons" and a "metric ton" equals 1000 "bushels", or some such. These rules can be used both for conversion and for composition/decomposition of _Goods_ into workable quantities.
;;; 
;;; ### Fitness
;;; 
;;; Properties are used by _Sellers_ to identify their _Goods_ for sale and to support their "fitness" for some purpose (oranges of a certain moisture content might be more amenable to juicing, while others are better for out-of-hand eating), and for _Buyers_ to identify _Goods_ which meet their real-world needs. The combination of "fitness" and "price" determine the possibility for a sale of a given _Good_. The measure of fitness to defined by the Buyer, and can be a tight or loose in tolerance as they see fit, to provide them with the maximum flexibility in meeting their real-world needs.
;;; 
;;; 
;;; ### Clojure Implementation (enter "Whishful Thinking"*)
;;; 
;;; We'll start by defining a "good" as an empty hash-map and see where that gets us.
;;; 
;;; We will define a unit-of-measure as an empy hash-map as well, allowing us to define as many units as we'd like, whenever we decide we need a new one.
;;; 
;; **

;; **
;;; > *"wishful thinking" (see SICP) meaning we just assume something that works and play along, figuring out how to _actually_ make it work as we go
;; **

;; @@
(def good {})

(def unit-of-measure {})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.resource-mgmt/good</span>","value":"#'sicp.resource-mgmt/good"}
;; <=

;; **
;;; As a way of trying this out, let's try to model our Orange market:
;; **

;; @@
(def orange-units {:units [:truck :ton :bushel]
                   :conversions {:truck {:ton [(partial * 2) (partial / 2)]}
                                 :ton {:bushel [(partial * 1000) (partial / 1000)]}}})

(def oranges {:units-of-measure orange-units})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.resource-mgmt/oranges</span>","value":"#'sicp.resource-mgmt/oranges"}
;; <=

;; **
;;; We defined 3 units of measure: truck, ton, and bushel and we have 2 conversions between them. Note how each convertion has a pair of functions in the vector, one for going in each direction, so we can chain them arbitrarily.
;; **

;; @@
(get-in oranges [:units-of-measure :conversions :truck :ton])

((-> oranges
     (get-in [:units-of-measure :conversions])
     (get-in [:truck :ton])
     first)
 5)

; 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}
;; <=

;; **
;;; **TODO:** need to implement a search to chain the conversion together: truck -> ton -> bushel
;;; 
;;;  > or perhaps we do something to pre-process the data we are given to "fill in' any missing conversions 
;; **

;; **
;;; One thing we __DON'T__ want to consider a property  of a `Good` is the _Ask_ or _Bid_ price! These are not inherent properties of the _Good_, they are second-order assocaitions made only within the _Market_ based upon the _Sellers_ and _Buyers_, and are expected to change frequently. _Ask_ and _Bid_ really should be thier own contructs, and since we expect then to change, we should make them be ```atoms``` so we have control over thier state changes:
;; **

;; @@
(def ask (atom {}))

(def bid (atom {}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sicp.resource-mgmt/bid</span>","value":"#'sicp.resource-mgmt/bid"}
;; <=

;; **
;;; >Note: this means that Goods are _values_, once we create a Good it is fixed for all time. Maybe this isn't a good decision long-term (for example, oranges rot, so their properties might change over time), but we'll come back to that later
;;; 
;;; >Also note: We will come back to how we want to compose things. Do we want aks/bid to be singletons that hold all the pricing (what we have here), or shoud each _Good_ hold its own ask/id?compostion
;; **

;; **
;;; ### Coming Back to Fitness
;;; 
;;; Let's turn now to the notion of "fitness for purpose" and how a _Buyer_ can go about identifying _Goods_ in the _Market_ that might meet their needs.
;;; 
;;; 
;;; Fitness must be determines by the _Buyer_, beacuse only the Buyer can determine their _need_.
;;; 
;;; > see also [_"Markets Don't Supply According to Demand"_]() 
;;; 
;;; It is really the _need_ that determines how much the Buyer is willing to give the Sellar for 'Units' of 'Fit' 'Goods'
;;; 
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; ### Expiration
;;; 
;;; Some goods have a finite life-span, for example, and airline seat; once the plane takes off the seat can no longer be exchanged.
;;; 
;;; 
;;; 
;; **

;; **
;;; 
;;; ### Knapsack problem
;;; 
;;; Can this be spun as a knapsack filling problem; trying to find the best _Goods_ to fufill a collection of _Buyer's_ needs?
;;; 
;;; Or is it a knapsack problem from the _Seller's_ perspective, trying to match their _Goods_ to the needs of the market?
;;; 
;;; Or is it the _Market's_ job, matching _Buyers_ to _Sellers_ and vice versa? i.e., [Arbitrage](https://en.wikipedia.org/wiki/Arbitrage)
;;; 
;;; 
;;; ### "language" is the key
;;; 
;;; I have no idea what  was thinking about this topic. WHich language, computer language? Human language? What?
;;; 
;;; 
;;; # Other topics of interest
;;; 
;;; relational algebra, geometric algebra
;;; 
;;; Is there such a thing as "compositional algebra"? Not composition of functions, but composition of entitites (left, right, above, in-front, part-of, property-of, etc)?
;;; 
;;; Not "if/then" but "when/do"
;;; 
;;; Type checkers are just Predicate validators. There is no magic here to address "real" Semantics
;;; 
;; **

;; **
;;; 
;; **
