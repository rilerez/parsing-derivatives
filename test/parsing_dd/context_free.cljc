(ns parsing-dd.context-free
  (:refer-clojure :exclude [cat])
  (:require [parsing-dd.context-free :as cf
             :refer [δ? D Ds matches]
             #?(:clj :refer :cljs :refer-macros) [cat alt rep]]
            [clojure.test :as test
             #?(:clj :refer :cljs :refer-macros) [is]]))


(defn map-nodes [f tree]
  (let [fed-tree (f tree)]
    (if (seqable? fed-tree)
      (map #(map-nodes f %) fed-tree)
      fed-tree)))
(defn force-all [tree] (map-nodes force tree))


(is (δ? (alt :ε :ε)))
(is (δ? (alt :ε [:□ :c])))
(is (not (δ? (alt :∅ [:□ :c]))))

(def list-of-x (alt (cat list-of-x [:□ :x]) :ε))
(is (δ? (D :x list-of-x)))

(def balanced (alt (cat balanced [:□ :d] balanced [:□ :b])
                   :ε))
(is (matches? balanced [:d :b]))
(is (matches? balanced [:d :b :d :d :b :b]))
(is (not (matches? balanced [:d :b :d :d :b])))

