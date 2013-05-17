
(ns smogon.dex
  (:require [clojure.core.logic :as l]
            [clojure.tools.macro :as m]
            [smogon.core-logic-hacks :as lhacks]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [smogon.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private vec-union [x y]
  (vec (sort (concat x y)))) 

(defn ^:private group-vals-vec [coll]
  (reduce (fn [m [k v]] (merge-with vec-union m {k #{v}})) {} coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All dex objects :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel name-r ^:index o x)

(defn name-of [x]
  (lhacks/run-1 [q] (name-r x q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def official-gens
  '(:rb :gs :rs :dp :bw))

(defn gens->ordering
  [gens]
  (into {} (map-indexed #(vector %2 %1) gens)))

(defn ogens-since
  [gen]
  (drop-while #(not= gen %) official-gens))

(defn ogens-upto
  [gen]
  (take-while #(not= gen %) official-gens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel type-r ^:index g ^:index t)
(lhacks/defrel type-effective-against-r ^:index g ^:index t1 ^:index t2 modifier)

(defn type?
  ([t]
     (lhacks/run-bool (l/fresh [g] (type-r g t))))
  ([t g]
     (lhacks/run-bool (type-r g t))))

(defn type-effectiveness [t1 t2]
  (util/group-keys (l/run* [g q] (type-effective-against-r g t1 t2 q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel move-r ^:index g ^:index m)

(defn move?
  ([m]
     (lhacks/run-bool (l/fresh [g] (move-r g m))))
  ([m g]
     (lhacks/run-bool (move-r g m))))

(defn list-moves [] 
  (util/group-keys (lhacks/run-strict [g q] (move-r g q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel ability-r ^:index g ^:index a)

(defn ability?
  ([a]
     (lhacks/run-bool (l/fresh [g] (ability-r g a))))
  ([a g]
     (lhacks/run-bool (ability-r g a))))

(defn list-abilities [] 
  (util/group-keys (lhacks/run-strict [g q] (ability-r g q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel item-r ^:index g i)

(defn item?
  ([i]
     (lhacks/run-bool (l/fresh [g] (item-r g i))))
  ([i g]
     (lhacks/run-bool (item-r g i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pokemon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def official-gens-without-abilities #{:rb :gs})

(lhacks/defrel pokemon-r ^:index g ^:index x)
(lhacks/defrel pokemon-type-r ^:index g ^:index p ^:index x)
(lhacks/defrel pokemon-ability-r ^:index g ^:index p ^:index x)
(lhacks/defrel pokemon-egggroup-r ^:index g ^:index p ^:index x)
(lhacks/defrel pokemon-hp-r ^:index g ^:index p x)
(lhacks/defrel pokemon-atk-r ^:index g ^:index p x)
(lhacks/defrel pokemon-def-r ^:index g ^:index p x)
(lhacks/defrel pokemon-spatk-r ^:index g ^:index p x)
(lhacks/defrel pokemon-spdef-r ^:index g ^:index p x)
(lhacks/defrel pokemon-speed-r ^:index g ^:index p x)
(lhacks/defrel pokemon-weight-r ^:index g ^:index p x)
(lhacks/defrel pokemon-height-r ^:index g ^:index p x)

(defn pokemon?
  ([p]
     (lhacks/run-bool (l/fresh [g] (pokemon-r g p))))
  ([p g]
     (lhacks/run-bool (pokemon-r g p))))

(defn list-pokemon []
  (util/group-keys (l/run* [g q] (pokemon-r g q))))

(defn type-of [p]
  (util/group-keys (group-vals-vec (l/run* [g q] (pokemon-type-r g p q)))))

(defn has-type [t]
  (util/group-keys
   (lhacks/run-strict [g q]
                      (cond
                       (keyword? t) (pokemon-type-r g q t)
                       (coll? t) (l/everyg #(pokemon-type-r g q %) t)))))

(defn abilities-of [p]
  (util/group-keys (l/run* [g q] (pokemon-ability-r g p q))))

(defn has-ability [a]
  (util/group-keys (l/run* [g q] (pokemon-ability-r g q a))))

(defn hp-of [p]
  (util/group-keys (l/run* [g q] (pokemon-hp-r g p q))))

(defn atk-of [p]
  (util/group-keys (l/run* [g q] (pokemon-atk-r g p q))))

(defn def-of [p]
  (util/group-keys (l/run* [g q] (pokemon-def-r g p q))))

(defn spatk-of [p]
  (util/group-keys (l/run* [g q] (pokemon-spatk-r g p q))))

(defn spdef-of [p]
  (util/group-keys (l/run* [g q] (pokemon-spdef-r g p q))))

(defn speed-of [p]
  (util/group-keys (l/run* [g q] (pokemon-speed-r g p q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evolutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel evolves-r ^:index p1 ^:index p2)
(lhacks/defrel representative-r ^:index p rep)

(defn ^:private immediate-preevos-of
  "In the current generation, return the set of Pokemon that directly evolve into
  p."
  [p]
  (lhacks/run-set [q]
        (l/conde
         ((evolves-r q p)))))

(defn ^:private immediate-postevos-of
  "In the current generation, return the set of Pokemon that directly evolve from
  p."
  [p]
  (lhacks/run-set [q]
        (l/conde
         ((evolves-r p q)))))

(defn ^:private preevos-of
  [p]
  ((fn helper [s p]
     (cond
      (contains? s p) s
      :else (reduce helper (conj s p) (immediate-preevos-of p))))
   #{} p))

(defn ^:private postevos-of
  [p]
  ((fn helper [s p]
     (cond
      (contains? s p) s
      :else (reduce helper (conj s p) (immediate-postevos-of p))))
   #{} p))

(defn representative-of
  "The value returned by this function has no meaning in and of itself, but you
  can be assured that members of the same family share the same representative."
  [p]
  (lhacks/run-1 [p'] (representative-r p p')))

(defn related?
  [& ps]
  (->> ps
       (map representative-of)
       set
       count
       (= 1)))

(defn family-of
  "In graph theoretic terms, find the connected component p resides in, in the
  current generation."
  [p]
  ((fn helper
     [p avoid-nodes]
     (let [next (set (filter #(not (contains? avoid-nodes %))
                             (concat (immediate-preevos-of p)
                                     (immediate-postevos-of p))))
           avoid-nodes' (set/union avoid-nodes next)
           edges (set/union
                  (->> p immediate-preevos-of
                       (map #(vector % p))
                       set)
                  (->> p immediate-postevos-of
                       (map #(vector p %))
                       set))]
       (reduce (fn [[nodes edges] p']
                 (let [[nodes' edges'] (helper p' avoid-nodes')]
                   [(set/union nodes nodes')
                    (set/union edges edges')]))
               [#{p} edges]
               next))) p #{}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learnsets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lhacks/defrel learns-sans-preevos-r ^:index g ^:index p ^:index m)

(defn has-move [m]
  (util/group-keys (mapcat (fn [[g p]]
                        (for [postevo (postevos-of p)
                              :when (pokemon? postevo g)]
                          [g postevo]))
                      (l/run* [g q] (learns-sans-preevos-r g q m)))))

(defn moves-of [p]
  (util/group-keys (mapcat #(l/run* [g q] (learns-sans-preevos-r g % q))
                      (preevos-of p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in-gen
  "Given gen-summarized data, return only the entries valid in a specific gen."
  [g xgs]
  (let [xs (for [[x gs] xgs
                 :when (contains? gs g)]
             x)]
    (if (= (count xs) 1)
      (first xs)
      xs)))

(defn not-in-gen
  "Given gen-summarized data, return only the entries invalid in a specific gen."
  [g xgs]
  (into {} (remove #(contains? (second %) g) xgs)))

(defn relative-to-gen
  [g xgs]
  [(in-gen g xgs)
   (not-in-gen g xgs)])

(defn diff-gens
  "Assuming the input \"stacks\" with each gen, we will give the diffs."
  [xgs]
  (->> xgs
       (group-by second)
       (map (fn [[gs xgs]] [gs (map first xgs)]))
       (sort-by (fn [[gs xs]] (count xs)))
       (reduce (fn [[gens-so-far gsxs] [gs xs]]
                 [(set/union gs gens-so-far)
                  (cons [(set/difference gs gens-so-far) xs] gsxs)])
               ;; Initial state: no gens seen so far.
               [#{} []])
       second))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn ^:private make-generational
  "(make-generational :gs [[:ice :grass] :dp [:fire]] 
     --> 
   {:gs [:ice :grass], :rs [:ice grass], :dp [:fire], :bw [:fire]}"
  [begin-gen [v & {:as overrides}]]
  (let [gen-map (merge overrides {begin-gen v})
        gens (ogens-since begin-gen)] 
    (fill-gens gens gen-map)))

(defn deftypechart
  [id & {name :name
         gen :introduced-in
         geffectives :effective-against}]
  (doseq [g (ogens-since gen)]
    (l/fact type-r g id))
  (doseq [[g mods] (make-generational gen geffectives)
          [type mod] mods]
    (l/fact type-effective-against-r g id type mod))
  (l/fact name-r id name))

(defn defmove
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (ogens-since gen)]
    (l/fact move-r g id))
  (l/fact name-r id name))

(defn defability
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (ogens-since gen)]
    (l/fact ability-r g id))
  (l/fact name-r id name))

(defn defitem
  [id & {name :name,
         gen :introduced-in}]
  (doseq [g (ogens-since gen)]
    (l/fact item-r g id))
  (l/fact name-r id name))

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
  (doseq [g (ogens-since gen)]
    (l/fact pokemon-r g id))
  (l/fact name-r id name)
  (letfn [(mg [x] (make-generational gen x))]
    (doseq [[g types] (mg gtypes)
            type types]
      (l/fact pokemon-type-r g id type))
    (doseq [[g abilities] (mg gabilities)
            ability abilities]
      (when-not (contains? official-gens-without-abilities g) 
        (l/fact pokemon-ability-r g id ability)))
    (doseq [[g stats] (mg gstats)]
      ;; Ugly hack; for :rb spatk/spdef are combined.
      (if (= g :rb)
        (let [[hp atk def special speed] stats]
          (l/fact pokemon-hp-r g id hp)
          (l/fact pokemon-atk-r g id atk)
          (l/fact pokemon-def-r g id def)
          (l/fact pokemon-spatk-r g id special)
          (l/fact pokemon-spdef-r g id special)
          (l/fact pokemon-speed-r g id speed))
        (let [[hp atk def spatk spdef speed] stats]
          (l/fact pokemon-hp-r g id hp)
          (l/fact pokemon-atk-r g id atk)
          (l/fact pokemon-def-r g id def)
          (l/fact pokemon-spatk-r g id spatk)
          (l/fact pokemon-spdef-r g id spdef)
          (l/fact pokemon-speed-r g id speed))))
    (doseq [[g egggroups] (mg gegggroups)
            egggroup egggroups]
      (l/fact pokemon-egggroup-r g id egggroup))
    (doseq [[g weight] (mg gweight)]
      (l/fact pokemon-weight-r g id weight))
    (doseq [[g height] (mg gheight)]
      (l/fact pokemon-height-r g id height))))

(defn deflearnset
  [g & pairs]
  (doseq [[p ms] (partition 2 pairs)
          m ms]
    (l/fact learns-sans-preevos-r g p m)))

(defn familymerge
  [[n e] [n' e']]
  [(set/union n n') (set/union e e')])

(defn familyclique
  [& ps]
  [(set ps) (set (for [p ps
                       p' ps
                       :when (not= p p')]
                   [p p']))])

(defn familychain
  [& ps]
  [(set ps) (map vector ps (rest ps))])

(defn familyalts
  [f & ps]
  (reduce familymerge (map f ps)))

(defn deffamily
  [[nodes links]]
  (let [rep (first nodes)]
    (doseq [p nodes]
      (l/fact representative-r p rep))
    (doseq [[p pevo] links]
      (l/fact evolves-r p pevo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dex server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private load-dex-files
  "Load every .clj script under src/pokedb. The intention is to initialize the
  pokedex relations."
  []
  (doseq [f (file-seq (io/file (io/resource "pokedb/.")))]
    (when (.isFile f)
      (load-file (.getPath f)))))

(defn start-dex
  "Start up the dex; load up all the files necessary.

  Don't call this function directly; prefer smogon.core/start-all with
  :start-dex true."
  []
  (load-dex-files))
