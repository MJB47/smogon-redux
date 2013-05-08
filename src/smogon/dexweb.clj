
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
  [f [base diffs]]
  [:div (f base) (for [[v gens] diffs]
                   [:div (interpose "," (map #(render-gen %) gens)) " " (f v)])])

(defn render-pokemon-row [p]
  (let [d (dex/summarize-pokemon p)] 
    [:tr
     [:td (dex/name-of p)]
     [:td (render-diff render-types (:type d))]
     [:td (render-diff render-abilities (:abilities d))]
     [:td (render-diff render-stat (:hp d))] 
     [:td (render-diff render-stat (:atk d))]
     [:td (render-diff render-stat (:def d))]
     [:td (render-diff render-stat (:spatk d))]
     [:td (render-diff render-stat (:spdef d))]
     [:td (render-diff render-stat (:speed d))]]))

(defn list-pokemon
  [pokemon]
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
   (for [p (sort-by dex/name-of pokemon)]
     (render-pokemon-row p))])

(defroutes dex-routes
  (GET "/pokemon" [] (list-pokemon (dex/list-pokemon))))
