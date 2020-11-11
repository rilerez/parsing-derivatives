(ns parsing-dd.context-free
  (:refer-clojure :exclude [cat])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :refer [union]]))


(defn map-entry [x y] (first {x y}))
(defmacro defn-memo [name & rest]
  `(def ~name (memoize (fn ~(gensym name) ~@rest))))

(defmacro cat
  ([] :ε)
  ([x] x)
  ([x y]
   `[:o (delay ~x) (delay ~y)])
  ([x y & zs]
   `(cat ~x (cat ~y ~@zs))))
(defmacro alt
  ([] :∅)
  ([x] x)
  ([x y]
   `[:U (delay ~x) (delay ~y)])
  ([x y & zs]
   `(alt ~x (alt ~y ~@zs))))
(defmacro rep [x]
  `[:* (delay ~x)])

(defn-memo simplify [L]
  (match L
    [:U x y] (cond (= @x :∅) @y
                   (= @y :∅) @x
                   :else (alt (simplify @x) (simplify @y)))
    [:o x y] (cond (= @x :ε) @y
                   (= @y :ε) @x
                   (or (= @y :∅) (= @x :∅)) :∅
                   :else (cat (simplify @x) (simplify @y)))
    [:* x]   (if (#{:ε :∅} @x) :ε (rep (simplify @x)))
    _____    L))

(defn drop-until [pred? col] (drop-while (comp not pred?) col))
(defn adjacent-drop-until [pred? col]
  (map first (drop-until
              (fn [[x y]] (pred? x y))
              (map vector col (concat (rest col) nil)))))
(defn fix [f init] (first (adjacent-drop-until = (iterate f init))))

(defn δ1 [L {:keys [guesses watch? grammar]
             :or {guesses {} watch? #{} grammar {}}}]
  (if (watch? L) {:? (guesses L false) :Δwatch? #{L}}
      (let [new-watch? (conj watch? L)  ; to find cycles
            context    {:guesses guesses :watch? new-watch?}]
        (match L
          :∅            {:? false}
          :ε            {:? true}
          [:□ a]        {:? false}
          [:* _]        {:? true}
          [:eval x]     (δ1 (eval x) context)
          [:grammar x]  (δ1 (grammar x) context)
          [:U x y]      (let [{x-? :? x-Δwatch? :Δwatch?} (δ1 @x context)
                              {y-? :? y-Δwatch? :Δwatch?} (δ1 @y context)]
                          {:?       (or  x-? y-?)
                           :Δwatch? (union x-Δwatch? y-Δwatch?)})
          [:o x y]      (let [{x-? :? x-Δwatch? :Δwatch?} (δ1 @x context)
                              {y-? :? y-Δwatch? :Δwatch?} (δ1 @y context)]
                          {:?       (and x-? y-?)
                           :Δwatch? (union x-Δwatch? y-Δwatch?)})))))

(defn-memo δnext-context [{:keys [guesses watch?]
                           :or {guesses {} watch? #{}}
                           :as context}]
  (let [updates  (for [w watch?]
                   (assoc
                    (δ1 w (assoc context
                                 :guesses guesses
                                 :watch?  (disj watch? w)))
                    :input w))
        Δwatch   (reduce union (map :Δwatch? updates))
        Δguesses (map #(map-entry (:input %) (:? %)) updates)]
    (assoc context
           :guesses (into guesses Δguesses)
           :watch?  (into watch? Δwatch))))

(defn δ?
  ([L] (δ? L {}))
  ([L context]
   (get-in (fix δnext-context (assoc context :watch? #{L}))
           [:guesses L])))


(defn-memo D
  ([c L] (D c L {}))
  ([c L {grammar :grammar
         :or {grammar {}}
         :as context}]
   (match L
     :∅            :∅
     :ε            :∅
     [:□ a]        (if (= a c) :ε :∅)
     [:* x]        (cat (D c x) L)
     [:eval x]     [:eval `(D ~c ~x ~context)]
     [:grammar x]  (D c (grammar x) context)
     [:U x y]      (alt (D c @x) (D c @y))
     [:o x y]      (if (δ? @x)
                     (alt (cat (D c @x) @y) (D c @y))
                     (cat      (D c @x) @y)))))


(defn Ds [tokens L]
  (reduce #(simplify (D %2 %1)) (simplify L) tokens))
(defn matches? [L tokens] (δ? (Ds tokens L)))

;; Local Variables:
;; eval: (put-clojure-indent 'match 1)
;; End:
