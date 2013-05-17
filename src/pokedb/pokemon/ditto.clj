
(in-ns 'smogon.dex)


(defpokemon :ditto
  :name "Ditto"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ditto]]
  :abilities [[:limber]
              :bw [:imposter :limber]]
  :stats [[48 48 48 48 48]
          :gs [48 48 48 48 48 48]]
  :weight [4]
  :height [0.3])

(deffamily (familychain :ditto))

(deflearnset :rb
  :ditto [:transform])

(deflearnset :gs
  :ditto [:transform])

(deflearnset :rs
  :ditto [:transform])

(deflearnset :dp
  :ditto [:transform])

(deflearnset :bw
  :ditto [:transform])
