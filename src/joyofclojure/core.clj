(ns joyofclojure.core
  (:import [java.util.concurrent.atomic AtomicLong]
           )
  (:require [clojure.core :refer :all]
            [clojure.repl :refer :all]
            )
  )

;; This project is to keep track of learning from the joy of clojure book.


;;To throw an exception
;;(throw (RuntimeException. "Illegal!"))

;; try/catch/finally in clojure
(defn try-catch [f]
  [(try
     (f)
     (catch ArithmeticException e "No Dividing By Zero")
     (catch Exception e (str "You are so bad..." (.getCause e)))
     (finally (println
              "Returning..."))
     )
   ]
  )


(def a (AtomicLong. 1))


;; Use Boolean/valueOf to parse boolean values:

(def b (Boolean/valueOf "false"))


;; use nil? to find out if an item is nil or false....
(defn isItNilOrFalse []
  (when (nil? nil) "Its Nil, not False")
  )


;; empty collections are 'true' values in Clojure. i.e. (= [] true) --> true
;; Use seq on collections if you want [] to evaluate to false.
(defn testEmptyColl []
  (if (seq []) "True" "False")
  )

;; another example of using (seq coll):
(defn printSeq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))
    )
  )


;; destructuring vectors
;; positionally destructure, and get the value. Here we get the third value and ignore
;; the first two.
(defn destructVector1 [x]
  (let [[_ _ thirdval] x]
    thirdval
    )
  )

;; we can also get the remaining args into a separate sequence
(defn destructVector2 [x]
  (let [[firstval & more] x]
    (prn "First Value is: " firstval)
    (prn "Remaining values are: " more)
    )
  )

;; And we can have access to the entire arg list if required using :as keyword
(defn destructVector3 [x]
  (let [[firstval & more :as allVals] x]
    (prn "firstval is: " firstval)
    (prn "more is: " more)
    (prn "allVals is: " allVals)
    )
  )

;; And if we require a specific positional arg from a vector we can use associative destructuring:
(defn destructVector4 [x]
  (let [{thirdValue 2} x]
    (prn "This is the third value from the vector: " thirdValue)
    )
  )

;;destructuring maps
;; we can't positionally destructure maps. use :keys , :strs, or :sym to extract values:
(def testMap {:fahad 30 :asad 29 :rabeea 25 :talha 21})

(defn destructMap1 []
  (let [{:keys [fahad asad]} testMap]
    (prn "Fahad's age is: " fahad)
    (prn "Asad's age is:" asad)
    )
  )


;; can also provide default values for keys that aren't found in the map using :or
(defn destructMap2 []
  (let [{:keys [fahad asad job] :or {job "Software Developer"}} testMap ]
    (prn "Fahad's age is: " fahad ", and he works as: " job)
    (prn "Asad's age is: " asad ", and he works as: " job)
    )
  )

;; Finally all destructuring options above, can be done on the function args as well:
;; Look at syntax, no binding symbol required in the function args. Only destructuring syntax.
(defn destructFuncParams [{:keys [fahad]}]
  (prn "Fahad's age is: " fahad)
  )



;; precision with big floating point numbers, require 'M' prefix at the end.
;; This will create BigDecimal numbers, otherwise floating points are all Doubles
(def arbitraryLongFloatingPoint 3.1231231323131231313131123131331233M)


;;As numbers get large, clojure automatically converts the class to hold the appropriate result.
;;e.g. start with Integer, add an extremely large number, result is a Long..
(def startingWithInteger (Integer. 9))

(defn whatsMyClassNow []
  (let [x (+ startingWithInteger 90000000000000)]
    (prn "Type of startingWithInteger is: " (class startingWithInteger))
    (prn "Type of the result of additiong of large number to startingWithInteger is: " (class x))
    )
  )

;; keywords and symbols are not the same. Keywords always refer to themselves, and always evaluate true for both identity and equality. Not the same case for symbols.
;;keywords are identical and equal:
(defn areKeywordsEqualAndIdentical [] (let [x :fahad y :fahad]
                                        (prn "Is x & y equal?" (= x y))
                                        (prn "Is x & y identical?" (identical? x y))
                                        ))

;;Not the same for symbols. They are equal but not Identical!
(defn areSymbolsEqualAndIdentical [] (let [x 'goat y 'goat z x]
                                       (prn "Is x & y equal?" (= x y))
                                       (prn "Is x & y identical?" (identical? x y))
                                       (prn "Is x & z identical?" (identical? x z))
                                       (prn (meta x))
                                       (prn (meta y))
                                       ))

;; Reason symbols aren't identical, is because each symbol may have some meta-data associated with it, which may be different. Symbols only are identical when its exactly the same symbol. To associate metadata with a symbol use with-meta function:

(defn associateMetaDataWitSymbol [] (let [x (with-meta 'goat {:color "brown"})
                                          y (with-meta 'goat {:color "white"})
                                          ]
                                      [(= x y)
                                       (identical? x y)
                                       (meta x)
                                       (meta y)
                                       ]
                                      ))


;; keywords can have namespaces attached to them. They can be referred like this :<namespace>/<keyword> e.g.
(defn examplesOfKeywords []
  (let [namespaceQualifiedKeyword :joyofclojure.core/lovelykeyword
        unqualifiedKeyword        :normalkeyword
        differentNameSpaceKeyword :some.important.namespace/contextdependentkeyword
        checkIfKeywordaExistsInNs (fn [n k] (map #(find-keyword n %) (map name k)))
        ]
    (prn "Only fully qualified keywords exists in joyofclojure.core ns: ")
    (prn (checkIfKeywordaExistsInNs "joyofclojure.core" [namespaceQualifiedKeyword unqualifiedKeyword differentNameSpaceKeyword]))
    (prn "Even if current namespace is joyofclojure.core other keywords in different namespaces can be resolved:")
    (prn (checkIfKeywordaExistsInNs "some.important.namespace" [differentNameSpaceKeyword unqualifiedKeyword]))
    (prn "but unqualifiedkeywords don't exist in any namespace: ")
    (prn (checkIfKeywordaExistsInNs "" [unqualifiedKeyword]))
    )
   )


;; clojure composite data types / collections are of three 'partitions': sequentials, maps and sets
;; sequentials will be equal to each other, if the contents and the order are the same:
;; Everything that implements java.util.List is sequential
(defn listsAndVectorsAreEqual? []
  (= [1 2 3] '(1 2 3))
  )

;; However, unordered and ordered collections won't be equal even if contents are the same:
(defn listsAndSetsAreEqual? []
  (= '(1 2 3) #{1 2 3})
  )

;; seq abstraction, has 3 key functions:
;; seq - to convert a collection to sequence. seq on empty collection gives nil.
;; first - to give the first element or nil
;; rest  - to give everything but the first element or empty seq.
(defn sequenceTesting []
  (let [convertingToSeq (seq "Fahad")
        firstOnEmptySeq (first '())
        restOnEmptySeq  (rest '())
        ]

    (prn "Seq can be used to convert collections and strings to sequences: " convertingToSeq)
    (prn "But first on empty seq gives you nil: " firstOnEmptySeq)
    (prn "However rest on empty seq gives you empty seq: " restOnEmptySeq)
    )
  )


;; Vectors are efficient in three things over lists:
;; 1) adding / removing items at the end of the collection
;; 2) searching by numerical index in constant time (like arrays)
;; 3) walking in reverse order (use rseq)
;;
;; you can fetch items from vector using nth, get or using vector as a function
;; nth and get can also provide a default value if the indexed item isn't found.
;;
;; can replace item in place using assoc function.
;; can use replace function to replace specific values with other values. replace works with seqs
;; and vectors.
(defn vectors101 []
  (let [a [1 2 3]
        thirdItem (nth a 2)
        thirdItemUsingGet (get a 2)
        fetchingDefaultValueIfItemNotFound (nth a 10 :NOTFOUND)
        addedNewtoEnd (conj a 4)
        reversedSeq (rseq a) ;; efficient in vectors
        assocedVector (assoc a 2 :WhatHappenedHere!)
        replacedVector (replace {2 :a 3 :b} a)
        ]
    (println fetchingDefaultValueIfItemNotFound)
    (println reversedSeq)
    (println assocedVector)
    (println replacedVector)
    )
  )

;; Vectors can be created programmatically using vec and into functions:
;; can also use a vector-of function to choose what type of values are stored in the vector:
;; vector-of [:int :char :long :float :double :byte :short :boolean]
(defn buildingVectors []
  (do
    ;; can build vectors using vec function which takes a seq or arbitrary collection and converts it to a vector:
    (vec (range 10) )
    ;; can add items to an existing vector using into function:
    (into [1 2 3] '(4 5))
    ;; or can specify type of values. for instance here only 2 is a valid int, others are float
    ;; but the resulting vector casts everything to int
    (into (vector-of :int) [Math/PI 2 1.3])
    ;; or can cast ints to chars:
    (into (vector-of :char) [100 101 102])
    ;; but the limits of the individual container types apply otherwise the items can be out of rante:
    (into (vector-of :int) [122222112121212122121212121212121])
    )
  )


;; Working with nested vectors.
;; get-in can be used to retreive values from multi-dimensional vectors.
;; assoc-in and update-in functions can be used for multi-dimensional vectors
;; update-in doesn't overwrite value, instead it takes a function and applies that to the specific
;; location.
(defn nestedVectors []
  (let [a [[1 2 3]
           [4 5 6]
           [7 8 9]]

        middleValue (get-in a [1 1]) ;; 1st value in vector is the first vector index, second is the value index.
        changeMiddleValue (assoc-in a [1 1] \x)
        updateMiddleValue (update-in a [1 1] #(* 100 %))
        ]
    (println middleValue)
    (println changeMiddleValue)
    (println updateMiddleValue)
    )
  )


;; Vectors can be used as a stack too.
;; To push items on to vector use conj
;; To pop items from the vector use pop
;; To lookup the topmost stack value use peek
(defn stack101 []
  (let [a [1 2 3]
        pushItemToStack (conj a 4)
        popItemOnStack (pop a)
        peeking (peek pushItemToStack)
        ]
    (println [pushItemToStack popItemOnStack peeking])
    )
  )

;; Map Entries are Vectors too, so very handy to use vector functions on maps
;; use doseq to iterate and bind values using destructuring, very nice!
;; each MapEntry (key, val pair) is actually a vector. So dimension and value takes the key
;; and value respectively.
(defn doseq101 []
  (doseq [[dimension value] {:width 10 :height 20 :length 30}]
    (println (str (name dimension) ": " value " inches"))
    )
  )

;; Lists can be built using cons or conj or list function. conj adds values to the front of the list rather than
;; back of the vector. cons is an abstraction of value + seq, so the order of arguments are
;; different.
;; LIsts can also be used as stack, except that the items are added/removed from the front of
;; the list rather than the end as in a vector.
(defn lists101 []
  (let [a '(1 2 3)
        b (cons :consedValue a)
        c (conj a :first :second :third)
        d (list :a :b :c)]
    (println [a b c d])
    )
  )


;;Queues in Clojure are created by adding items to the EMPTY QUEUE:
;; clojure.lang.PersistentQueue/EMPTY
;; As with Stacks use conj, pop and peek to operate on queue.
;; Main different, queues are FIFO whereas stacks are LIFO
(defn queues101 []
  (let [myFirstQueue clojure.lang.PersistentQueue/EMPTY
        addingItemsToQueue (conj myFirstQueue :first :second :third)
        removingItemsFromQueue (pop addingItemsToQueue)
        lookingAtHeadOfTheQueue (peek addingItemsToQueue)
        ]
    [myFirstQueue addingItemsToQueue removingItemsFromQueue lookingAtHeadOfTheQueue]
    )
  )


;; the above  queues don't print nicely on console. apparently this 'magic multimethod' code
;; that I don't understand makes it print nicely. Comment out the below function to see how the
;; queues print without it in effect!
(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w)
  )


;;Sets in clojure are represented using # and {}, and can be used as functions to test if the value;; is contained in the set or not.
;; can use 'get' function to query set values.
;; Also can use contains? function to test if the value exists in Sets BUT NOT IN LISTS/VECTORS.
;; Reason is because contains? function tests for keys rather than values. And in Clojure all Set
;; values are keys also. So Contains? doesn't work as well on vectors.
(defn fetchingValuesFromSets []
  (let [a #{1 2 3 4}
        valueUsingSETAsFunction (a 2) ;; doesn't return third value, it checks to return the value 2 if it exists or nil.
        valueUsingGetAsFunction (get a 5 :uhoh) ;; can provide default values like with vecs/lists
        containsOnVec (contains? [1 2 3 4] 3) ;; This will be false! as lists contains vals only.
        containsOnSet (contains? a 3)
        ]
    [a valueUsingSETAsFunction valueUsingGetAsFunction containsOnVec containsOnSet]
    )
  )


;; All Set operations are in clojure.set namespace.
;; intersect -> items in both sets only
;; union -> a combined set with all items in both sets
;; difference -> subtracts all elements in set A that are also in set B.
(defn setOperations []
  (let [A #{:apple :orange :banana}
        B #{:red :blue :orange}
        intersection (clojure.set/intersection A B)
        unionisation (clojure.set/union A B)
        differencisation (clojure.set/difference A B)
        ]
    [intersection unionisation differencisation]
    )
  )


;; there are multiple map implementations in clojure:
;; map literal defaults to array-map i.e. {:a 1} - use array map if you want to maintain insertion
;; order.
;; can also use sorted-map
;; otherwise can create hash-map - doesn't keep insertion order.
;; maps can be used as functions, and accept keys as values
;; Also seq on maps return key/value pair vectors.
;; seq'ed map key/value vectors can be converted back into maps using 'into' function
;; Even if the values aren't vectors they can be coverted into vectors
;; Or can 'apply' hash-map function to a vector of values. The sequence defines key/values.
;; this idiom also extends to zipmap which zips two vectors together.
(defn maps101 []
  (let [arrayMapImpl (array-map :a 1 :b 2)
        hashMapImpl (hash-map :a 1 :b 2)
        findA (arrayMapImpl :a)
        seqOnMap (seq hashMapImpl)
        convertSeqBackToMap (into {} seqOnMap)
        convertListToVecToMap (into {} (map vec '[(1 2) (3 4)]))
        applyHashMapToVector (apply hash-map [\a \b \c \d])
        applyZipMapTo2Vectors (zipmap [:APPLE :BANANA] [1 2])
        ]
    (println "Finding :a using map as key result:" findA)
    (println "doing seq on map: " seqOnMap)
    (println "converting seq back to map: " convertSeqBackToMap)
    (println "converting lists to vectors to maps: " convertListToVecToMap)
    (println "apply hash-map function to vector of values: " applyHashMapToVector)
    (println "apply zipmap fn to two vectors: " applyZipMapTo2Vectors)
    (map type [arrayMapImpl hashMapImpl])
    )
  )


;; Sorted Maps sort the keys and insert them into the map.
;; Can use sorted-map-by fn to provide custom comparator for the keys.
;; Sorted Maps treat number values the same even if the type of number is different float vs int.
(defn sortedMaps101 []
  (let [
        a (sorted-map "bac" 2 "abc" 3)
        usingCustomComparator (sorted-map-by #(compare (subs %1 1) (subs %2 1)) "bac" 2 "abc" 3)
        b (assoc (sorted-map 1 :int) 1.0 :float)
        ]

    (println "Sorted map sorts keys in natural order: " a)
    (println "Using customer comparator allows for changing order: " usingCustomComparator)
    (println "Be careful when using numbers for keys in sorted map: " b)
    )
  )
