(ns parsing-dd.parser
  (:refer-clojure :exclude [cat])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :refer [union]]))

(defn map-entry [x y] (first {x y}))

(defn drop-until [pred? col] (drop-while (comp not pred?) col))
(let [last (gensym 'last)]
 (defn adjacent-drop-until [pred? col]
   (map first (drop-until
               (fn [[x y]] (if (= y last) false
                               (pred? x y)))
               (map vector col (concat (rest col) last))))))
(defn fix [f init] (first (adjacent-drop-until = (iterate f init))))

(defmacro defn-memo [name & rest]
  `(def ~name (memoize (fn ~(gensym name) ~@rest))))

(defmacro cat
  ([] :ε)
  ([x] x)
  ([x y]      `[:o (delay ~x) (delay ~y)])
  ([x y & zs] `(cat ~x (cat ~y ~@zs))))
(defmacro alt
  ([] :∅)
  ([x] x)
  ([x y]      `[:U (delay ~x) (delay ~y)])
  ([x y & zs] `(alt ~x (alt ~y ~@zs))))
(defmacro rep [x]   `[:* (delay ~x)])
(defmacro =>  [x f] `[:=> (delay ~x) ~f])
(defmacro δ   [x]   `[:δ (delay ~x)])

;; parses:
;; set of seq of trees
;; U       o

(defn-memo D
  ([c P] (D c P {}))
  ([c P {grammar :grammar
         :or {grammar {}}
         :as context}]
   (match P
     :∅            :∅
     [:ε↓ _]       :∅
     [:δ _]        :∅
     [:□ a]        (if (= a c) [:ε↓ #{[a]}] :∅)
     [:* x]        (cat (D c @x) P)
     [:=> p f]     (=> (D c @p context) f)
     [:U x y]      (alt (D c @x) (D c @y))
     [:o x y]      (alt (cat (D c @x) @y)
                        (cat (δ @x)   (D c @y)))
     [:eval x]     [:eval `(D ~c ~x ~context)]
     [:grammar x]  (D c (grammar x) context))))

(defn-memo parse-ε1 [P {:keys [guesses    watch?]
                        :or   {guesses {} watch? #{}}}]
  (if (watch? P)
    {:? (guesses P #{}) :Δwatch? #{P}}
    (let [new-watch? (conj watch? P)
          context    {:guesses guesses :watch? new-watch?}
          rec        #(parse-ε1 % context)]
      (match P
        :∅        {:? #{}}
        [:ε↓ T]   {:? T}
        [:δ x]    (rec @x)
        [:□ _]    {:? #{}}
        [:U X Y]  (let [PX (rec X), PY (rec Y)]
                    {:?       (concat (:? PX) (:? PY))
                     :Δwatch? (union (:Δwatch? PX) (:Δwatch? PY))})
        [:o X Y]  (let [PX (rec X), PY (rec Y)]
                    {:?       (for [x (:? PX), y (:? PY)] (concat x y))
                     :Δwatch? (union (:Δwatch? PX) (:Δwatch? PY))})
        [:=> X f] (let [PX (rec X)]
                    {:?       (map f (:? PX))
                     :Δwatch? (:Δwatch? PX)})
        [:* _]    {:? #{()}}))))

(defn-memo parse-ε-next-context [{:keys [guesses    watch?]
                                  :or   {guesses {} watch? #{}}
                                  :as context}]
  (let [updates  (for [w watch?]
                   (assoc
                    (parse-ε1 w (assoc context
                                       :guesses guesses
                                       :watch?  (disj watch? w)))
                    :input w))
        Δwatch   (reduce union (map :Δwatch? updates))
        Δguesses (map #(map-entry (:input %) (:? %)) updates)]
    (assoc context
           :guesses (into guesses Δguesses)
           :watch?  (into watch? Δwatch))))

(defn parse-ε
  ([P] (parse-ε P {}))
  ([P context]
   (get-in (fix parse-ε-next-context
                (assoc context :watch? #{P}))
           [:guesses P])))

(defn force-root
  "Force evaluation of the delayed slots in the root of the tree P.
  Useful for pattern matching."
  [P]
  (match P
    [:o x y] [:o @x @y]
    [:U x y] [:U @x @y]
    [:δ x]   [:δ @x]
    [:* x]   [:* @x]
    [:=> x]  [:=> @x]
    _______  P))

(defn-memo simplify1 [P]
  (match (force-root P)
    (:or [:o :∅ _]
         [:o _ :∅])      :∅
    [:U :∅ p ]           (simplify1 p)
    [:U p  :∅]           (simplify1 p)
    [:o [:ε↓ trees] p]   (=> (simplify1 p) (fn [t] (map #(concat % t) trees)))
    [:o p [:ε↓ trees]]   (=> (simplify1 p) (fn [t] (map #(concat t %) trees)))
    [:=> [:ε↓ trees] f]  [:ε↓ (map f trees)]
    [:=> [:=> p f] g]    (=> (simplify1 p) (comp g f))
    [:* :∅]              [:ε↓ #{[]}]
    ___________________  P))

(defn simplify [P] (nth 2 (iterate simplify1 P)))

(defn Ds [tokens P]
  (reduce #(simplify (D %2 %1)) (simplify P) tokens))

(defn parse [P tokens] (parse-ε (Ds tokens P)))

;; Local Variables:
;; eval: (put-clojure-indent 'match 1)
;; End:
