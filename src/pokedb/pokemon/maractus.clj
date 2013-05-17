
(in-ns 'smogon.dex)


(defpokemon :maractus
  :name "Maractus"
  :introduced-in :bw
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll :storm-drain :water-absorb]]
  :stats [[75 86 67 106 67 60]]
  :weight [28]
  :height [1])

(deffamily (familychain :maractus))

(deflearnset :bw
  :maractus [:absorb :acupressure :aerial-ace :after-you :attract :bounce :bullet-seed :cotton-guard :cotton-spore :double-team :drain-punch :endeavor :energy-ball :facade :frustration :giga-drain :grass-knot :grasswhistle :growth :helping-hand :hidden-power :hyper-voice :ingrain :knock-off :leech-seed :mega-drain :needle-arm :peck :petal-dance :pin-missile :poison-jab :protect :rest :return :round :safeguard :seed-bomb :sleep-talk :snore :solarbeam :spikes :substitute :sucker-punch :sunny-day :swagger :sweet-scent :synthesis :toxic :uproar :wood-hammer :worry-seed])

