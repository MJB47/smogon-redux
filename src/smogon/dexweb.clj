
(ns smogon.dexweb
  "Web interface to the smogdex."
  (:require [smogon.web-helpers :refer :all]
            [smogon.dex :as dex]
            [clojure.string :as s]))

(defn render-gen [g]
  (case g
    :rb "RB"
    :gs "GS"
    :rs "RS"
    :dp "DP"
    :bw "BW"))

(defn render-type [t]
  [:a (dex/name-of t)])

(defn render-types [ts]
  [:div (interpose "/" (map render-type ts))])

(defn render-ability [a]
  [:a (dex/name-of a)])

(defn render-abilities [as]
  [:span (interpose "/" (map render-ability as))])

(defn render-stat [s]
  (str s))

(defn render-diff
  [f diffs]
  [:div (for [[v gens] diffs] [:div (interpose "," (map #(render-gen %) gens)) " " (f v)])])

(defn render-pokemon-row [p gens]
  [:tr
   [:td (dex/name-of p) [:br] (s/join "," (map render-gen gens))]
   [:td (render-diff render-types (dex/type-of p))]
   [:td (render-diff render-ability (dex/abilities-of p))]
   [:td (render-diff render-stat (dex/hp-of p))] 
   [:td (render-diff render-stat (dex/atk-of p))]
   [:td (render-diff render-stat (dex/def-of p))]
   [:td (render-diff render-stat (dex/spatk-of p))]
   [:td (render-diff render-stat (dex/spdef-of p))]
   [:td (render-diff render-stat (dex/speed-of p))]])

(defn list-pokemon
  [pokegens]
  [:table
   [:tr
    [:th "Name"]
    [:th "Type"]
    [:th "Abilities"]
    [:th "HP"]
    [:th "Atk"]
    [:th "Def"]
    [:th "SpA"]
    [:th "SpD"]
    [:th "Spe"]]
   (for [[p gens] (sort-by (comp dex/name-of first) pokegens)]
     (render-pokemon-row p gens))])

(defroutes dex-routes
  (GET "/pokemon" [] (list-pokemon (dex/list-pokemon))))
