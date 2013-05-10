
(ns smogon.core-logic-hacks
  "Because core.logic isn't always sane."
  (:require [clojure.core.logic :as l]
            [clojure.tools.macro :as m]))

(defmacro rel
  "Create an anonymous relation.

  This is a ridiculous hack because there is no way to create a relation without
  binding it to a var."
  [& args]
  (let [relsym (with-meta (gensym "rel") {:private true})]
    `(do
       (l/defrel ~relsym ~@args)
       ~relsym)))

(defmacro defrel
  "defrel with defonce semantics and docstrings."
  [name & args]
  `(defonce ~name (rel ~@args)))

(defmacro run-set [args & body]
  `(set (l/run* ~args ~@body)))

(defmacro run-1 [args & body]
  `(first (l/run 1 ~args ~@body)))

;; Warning: this is not the same as not-empty because Clojure is stupid as fuck
;; and thinks returning the list is accetpable answer (hint: its not, return
;; true or false please)
(defmacro run-bool [& body]
  `(not (empty? (l/run 1 [unused#] ~@body))))

(defmacro run-strict [args & body]
  `(doall (l/run* ~args ~@body)))
