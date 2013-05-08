
(ns smogon.dex
  (:require [clojure.core.logic :as l]
            [clojure.tools.macro :as m]
            [smogon.core-logic-hacks :as lhacks]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private load-dex-files
  "Load every .clj script under src/pokedb. The intention is to initialize the
  pokedex relations."
  []
  (doseq [f (file-seq (io/file (io/resource "pokedb/.")))]
    (when (.isFile f)
      (load-file (.getPath f)))))

(defn start-dex
  "Don't call this function directly; prefer smogon.core/start-all with
  :start-dex true"
  []
  (load-dex-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All dex objects :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel name-r ^:index o x)

(lhacks/defsingleton name-of [id] 
  [q] (name-r id q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *gen*
  "Current generation (for convenience). Defaults to latest generation."
  :bw)

(def official-gens
  '(:rb :gs :rs :dp :bw))

(defn gens->ordering
  [gens]
  (into {} (map-indexed #(vector %2 %1) gens)))

(defn gens-since
  [gen]
  (drop-while #(not= gen %) official-gens))

(defn gens-upto
  [gen]
  (take-while #(not= gen %) official-gens))

(defmacro in-gen
  "Evaluate body in the context of a different generation."
  [gen & body]
  `(binding [*gen* ~gen]
    ~@body))

(defn in-gens*
  [gens f]
  (into {} (for [gen gens
                 :let [v (in-gen gen (f))]
                 :when v]
             [gen v])))

(defmacro in-gens
  "Evaluate body in the context of each generation, and collect the results in a
  map keyed by generation."
  [gens & body]
  `(in-gens* ~gens (fn [] ~@body)))

(defn group-gens
  "(group-gens {:rb 3, :gs 4, :rs 5, :dp 3, :bw 4}) -> 
   {3 #{:rb :dp}, 4 #{:gs :bw}, 5 #{:rs}}"
  [genval]
  (reduce-kv (fn [cur gen val]
               (merge-with clojure.set/union {val #{gen}} cur))
             {}
             genval))

(defn ungroup-gens
  "(ungroup-gens {3 #{:rb :dp}, 4 #{:gs :bw}, 5 #{:rs}}) -> 
   {:rb 3, :gs 4, :rs 5, :dp 3, :bw 4}"
  [valgens]
  (reduce-kv (fn [cur val gens]
               (reduce #(assoc %1 %2 val) cur gens))
             {}
             valgens))

(defn in-gens-relative*
  [gens f]
  (let [genvals (in-gens* gens f)
        ;; Sort the results by generation
        valgens (sort-by (fn [[val gs]]
                           (reduce min (map (gens->ordering gens) gs)))
                         (group-gens genvals))
        [[v _] & rest] valgens]
    [v rest]))

(defmacro in-gens-relative
  "Return a vector [base-val diffs] where diffs is a list of [val gens] pairs.

  (in-gens-relative official-generations (abilities-of :clefable))
  [(:cute-charm) ([(:magic-guard :cute-charm) #{:dp}] [(:unaware :cute-charm :magic-guard) #{:bw}])]"
  [gens & body]
  `(in-gens-relative* ~gens (fn [] ~@body)))

(defn ^:private fill-gens
  "(fill-gens [:gs :rs :dp :bw] {:gs [:ice :grass], :dp [:fire]}) 
     --> 
   {:gs [:ice :grass], :rs [:ice grass], :dp [:fire], :bw [:fire]}"
  [[prev-gen & [gen :as next-gens]] gen-map]
  (cond
    (nil? gen) gen-map

    :else
    (let [;; Get the value from the previous generation,
          prev (get gen-map prev-gen)
          ;; ... and carry it over to the current generation (if the current
          ;; generation has no set value--merge works left to right)
          gen-map' (merge {gen prev} gen-map)]
      (fill-gens next-gens gen-map'))))

(defn make-generational
  "(make-generational :gs [[:ice :grass] :dp [:fire]] 
     --> 
   {:gs [:ice :grass], :rs [:ice grass], :dp [:fire], :bw [:fire]}"
  [begin-gen [v & {:as overrides}]]
  (let [gen-map (merge overrides {begin-gen v})
        gens (gens-since begin-gen)] 
    (fill-gens gens gen-map)))

;; Relations
;;

(defmacro defgenrel [name & args]
  (let [[name [& args]] (m/name-with-attributes name args)] 
    `(let [rel# (lhacks/rel ^:index g# ~@args)] 
       (when (defonce ~name (fn [~@args]
                              (rel# *gen* ~@args)))
         (alter-meta! #'~name #(assoc % ::genrel rel#))
         #'~name))))

(defmacro genfact
  [gen rel & tuple]
  `(l/fact (-> #'~rel meta ::genrel) ~gen ~@tuple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgenrel type-r t)
(defgenrel type-effective-against-r t1 t2 modifier)

(lhacks/defpredicate type? [tid] (type-r tid))
(lhacks/defsingleton type-effectiveness [tid1 tid2]
  [q] (type-effective-against-r tid1 tid2 q))
(lhacks/defquery type-effectiveness-row [tid]
  [q r] (type-effective-against-r tid q r))

(defn deftypechart
  [id & {name :name
         gen :introduced-in
         geffectives :effective-against}]
  (doseq [g (gens-since gen)]
    (genfact g type-r id))
  (doseq [[g mods] (make-generational gen geffectives)
          [type mod] mods]
    (genfact g type-effective-against-r id type mod))
  (l/fact name-r id name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgenrel move-r m)

(lhacks/defpredicate move? [id] (move-r id))

(lhacks/defquery list-moves [] 
  [q] (move-r q))
  
(defn defmove
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (gens-since gen)]
    (genfact g move-r id))
  (l/fact name-r id name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgenrel ability-r a)

(lhacks/defpredicate ability? [id] (ability-r id))

(lhacks/defquery list-abilities [] 
  [q] (ability-r q))

(defn defability
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (gens-since gen)]
    (genfact g ability-r id))
  (l/fact name-r id name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgenrel item-r i)

(lhacks/defpredicate item? [id] (item-r id))

(defn defitem
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (gens-since gen)]
    (genfact g item-r id))
  (l/fact name-r id name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pokemon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def official-gens-without-abilities #{:rb :gs})

(defgenrel pokemon-r x)
(defgenrel pokemon-type-r ^:index p ^:index x)
(defgenrel pokemon-ability-r ^:index p ^:index x)
(defgenrel pokemon-egggroup-r ^:index p ^:index x)
(defgenrel pokemon-hp-r ^:index p x)
(defgenrel pokemon-atk-r ^:index p x)
(defgenrel pokemon-def-r ^:index p x)
(defgenrel pokemon-spatk-r ^:index p x)
(defgenrel pokemon-spdef-r ^:index p x)
(defgenrel pokemon-speed-r ^:index p x)
(defgenrel pokemon-weight-r ^:index p x)
(defgenrel pokemon-height-r ^:index p x)

;; Queries

(lhacks/defpredicate pokemon? [id] (pokemon-r id))

(lhacks/defquery list-pokemon [] 
  [q] (pokemon-r q))

(lhacks/defquery type-of [id] 
  [q] (pokemon-type-r id q)
  :post #(let [t (vec (sort %))]
           (when (not-empty t)
             t)))

(lhacks/defquery abilities-of [id] 
  [q] (pokemon-ability-r id q)
  :post #(let [as (sort %)]
           (when (not-empty as)
             as)))

(lhacks/defquery ability-monset-of [aid]
  [q] (pokemon-ability-r q aid))

(lhacks/defsingleton hp-of [id]
  [q] (pokemon-hp-r id q))

(lhacks/defsingleton atk-of [id]
  [q] (pokemon-atk-r id q))

(lhacks/defsingleton def-of [id]
  [q] (pokemon-def-r id q))

(lhacks/defsingleton spatk-of [id]
  [q] (pokemon-spatk-r id q))

(lhacks/defsingleton spdef-of [id]
  [q] (pokemon-spdef-r id q))

(lhacks/defsingleton speed-of [id]
  [q] (pokemon-speed-r id q))

(lhacks/defsingleton weight-of [id]
  [q] (pokemon-weight-r id q))

(lhacks/defsingleton height-of [id]
  [q] (pokemon-height-r id q))

(defn defpokemon
  "Helper function to define a Pokemon."
  [id & {name :name,
         gen :introduced-in,
         gstats :stats,
         gtypes :types,
         gabilities :abilities,
         gegggroups :egggroups,
         gweight :weight,
         gheight :height}]

  (doseq [g (gens-since gen)]
    (genfact g pokemon-r id))
  
  (l/fact name-r id name)
  
  (letfn [(mg [x] (make-generational gen x))]
    (doseq [[g types] (mg gtypes)
            type types]
      (genfact g pokemon-type-r id type))
    
    (doseq [[g abilities] (mg gabilities)
            ability abilities]
      (when-not (contains? official-gens-without-abilities g) 
        (genfact g pokemon-ability-r id ability)))
    
    (doseq [[g stats] (mg gstats)]
      ;; Ugly hack; for :rb spatk/spdef are combined.
      (if (= g :rb)
        (let [[hp atk def special speed] stats]
          (genfact g pokemon-hp-r id hp)
          (genfact g pokemon-atk-r id atk)
          (genfact g pokemon-def-r id def)
          (genfact g pokemon-spatk-r id special)
          (genfact g pokemon-spdef-r id special)
          (genfact g pokemon-speed-r id speed))
        (let [[hp atk def spatk spdef speed] stats]
          (genfact g pokemon-hp-r id hp)
          (genfact g pokemon-atk-r id atk)
          (genfact g pokemon-def-r id def)
          (genfact g pokemon-spatk-r id spatk)
          (genfact g pokemon-spdef-r id spdef)
          (genfact g pokemon-speed-r id speed))))
    
    (doseq [[g egggroups] (mg gegggroups)
            egggroup egggroups]
      (genfact g pokemon-egggroup-r id egggroup))
    
    (doseq [[g weight] (mg gweight)]
      (genfact g pokemon-weight-r id weight))
    
    (doseq [[g height] (mg gheight)]
      (genfact g pokemon-height-r id height))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evolutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgenrel directly-evolves-r p p')

(defn evolves-r [p p']
  (l/conde
   ((directly-evolves-r p p'))
   ((l/fresh [p'']
           (directly-evolves-r p p'')
           (evolves-r p'' p')))))

(lhacks/defquery preevos-of [id]
  [q] (evolves-r id q))

(lhacks/defquery postevos-of [id]
  [q] (evolves-r q id))
 
(lhacks/defquery family-of [id]
  [q] (l/conde
       ((evolves-r q id))
       ((evolves-r id q))))

(defn ^:private group-transitive
  "(group-transitive [1 2 3]) -> [[1 2] [2 3]]"
  [xs] (map vector xs (rest xs)))

(defn ^:private make-vector-if-not
  "Take a wild guess."
  [x] (if (vector? x) x [x]))

(defn ^:private fixup-family-tree
  [tree]
  (for [g official-gens]
    (let [tree' (for [x tree]
                  ;; (deffamily :sneasel :weavile) is shorthand for (deffamily [:sneasel] [:weavile])
                  (let [alternatives (make-vector-if-not x)]
                    ;; Ignore Pokemon not in this generation
                    (filter #(in-gen g (pokemon? %)) alternatives)))] 
      [g (group-transitive tree')])))

(defn deffamily
 "Define a family tree. 

  For example, (deffamily :bulbasaur :ivysaur :venusaur) creates evolution
  relations between [:bulbasaur :ivysaur], [:ivysaur :venusaur] in every
  generation. 

  To specify different evolution paths, you may enclose alternatives in a
  vector. For example, (deffamily :slowpoke [:slowbro :slowking]) creates
  evolution relations between [:slowpoke, :slowbro] and [:slowpoke :slowking] in
  every generation (sans the latter in :rb).

  Pokemon that did not exist in a particular generation are ignored."
  [& tree]
  (doseq [[g pairs] (fixup-family-tree tree)
          [palts pevoalts] pairs
          p palts
          pevo pevoalts]
    (genfact g directly-evolves-r p pevo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learnsets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgenrel learns-sans-preevos-r p m)

(defn learns-r
  [p m]
  (l/conde ((learns-sans-preevos-r p m))
           ((l/fresh [p']
                     (evolves-r p' p) 
                     (learns-sans-preevos-r p' m)))))

(lhacks/defpredicate learns? [pid mid]
  (learns-r pid mid))

(lhacks/defquery move-monset-of [mid]
  [q] (learns-r q mid))

(lhacks/defquery learnset-of [pid]
  [q] (learns-r pid q))

(defn deflearnset
  [gen & pairs]
  (doseq [[p ms] (partition 2 pairs)
          m ms]
    (genfact gen learns-sans-preevos-r p m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summarizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn summarize-pokemon
  "Summarize a Pokemon across generations."
  [p]
  (let [rgens (reverse official-gens)] 
    {:type (in-gens-relative rgens (type-of p))
     ;; this is intentionally not rgens, because abilities are always additions.
     :abilities (in-gens-relative official-gens (abilities-of p))
     :hp (in-gens-relative rgens (hp-of p))
     :atk (in-gens-relative rgens (atk-of p))
     :def (in-gens-relative rgens (def-of p))
     :spatk (in-gens-relative rgens (spatk-of p))
     :spdef (in-gens-relative rgens (spdef-of p))
     :speed (in-gens-relative rgens (speed-of p))}))
