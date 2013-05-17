
(in-ns 'smogon.dex)


(defpokemon :unown
  :name "Unown"
  :introduced-in :gs
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[48 72 48 72 48 48]]
  :weight [5]
  :height [0.5])

(deffamily (familychain :unown))

(deflearnset :gs
  :unown [:hidden-power])

(deflearnset :rs
  :unown [:hidden-power])

(deflearnset :dp
  :unown [:hidden-power])

(deflearnset :bw
  :unown [:hidden-power])
