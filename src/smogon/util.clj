
(ns smogon.util
  (:require [clojure.set :as set]))

(defn sortv [x]
  (vec (sort x)))

(defn group-bare [f coll]
  (vals (group-by f coll)))

(defn group-vals [coll]
  (reduce (fn [m [k v]] (merge-with set/union m {k #{v}})) {} coll))

(defn group-keys [coll]
  (reduce (fn [m [k v]]  (merge-with set/union m {v #{k}})) {} coll))

;; FIXME inefficient
(defn min-by
  [f xs]
  (first (sort-by f xs)))
