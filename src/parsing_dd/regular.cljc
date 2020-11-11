(ns parsing-dd.regular
  (:require [clojure.core.match :refer [match]]))


(def δ?
  (fn [L]
    (match L
      :∅       false
      :ε       true
      [:□ a]   false
      [:* _]   true
      [:U x y] (or  (δ? x) (δ? y))
      [:o x y] (and (δ? x) (δ? y)))))

(defn cat [x y] [:o x y])
(defn alt [x y] [:U x y])
(defn rep [x] [:* x])
(def D
  (fn [c L]
    (match L
      :∅       :∅
      :ε       :∅
      [:□ a]   (if (= a c) :ε :∅)
      [:* x]   (cat (D c x) L)
      [:U x y] (alt (D c x) (D c y))
      [:o x y] (if (δ? x)
                 (alt (cat (D c x) y) (D c y))
                 (cat (D c x) y)))))
(δ? (D :x (D :x (cat [:□ :x] [:□ :x]))))
