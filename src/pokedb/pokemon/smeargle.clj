
(in-ns 'smogon.dex)


(defpokemon :smeargle
  :name "Smeargle"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:own-tempo]
              :dp [:own-tempo :technician]
              :bw [:moody :own-tempo :technician]]
  :stats [[55 20 35 20 45 75]]
  :weight [58]
  :height [1.2])

(deffamily (familychain :smeargle))

(deflearnset :gs
  :smeargle [:sketch])

(deflearnset :rs
  :smeargle [:sketch])

(deflearnset :dp
  :smeargle [:sketch])

(deflearnset :bw
  :smeargle [:sketch])
