
(in-ns 'smogon.dex)


(defpokemon :sunkern
  :name "Sunkern"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :solar-power]
              :bw [:chlorophyll :early-bird :solar-power]]
  :stats [[30 30 30 30 30 30]]
  :weight [1.8]
  :height [0.3])

(defpokemon :sunflora
  :name "Sunflora"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :solar-power]
              :bw [:chlorophyll :early-bird :solar-power]]
  :stats [[75 75 55 105 85 30]]
  :weight [8.5]
  :height [0.8])

(deffamily (familychain :sunkern :sunflora))



(deflearnset :gs
  :sunkern [:absorb :attract :curse :cut :double-team :endure :flash :frustration :giga-drain :growth :hidden-power :mega-drain :protect :rest :return :sleep-talk :sludge-bomb :snore :solarbeam :sunny-day :swagger :sweet-scent :synthesis :toxic]
  :sunflora [:hyper-beam :petal-dance :pound :razor-leaf])

(deflearnset :rs
  :sunkern [:absorb :attract :bullet-seed :curse :cut :double-edge :double-team :encore :endeavor :endure :facade :flash :frustration :giga-drain :grasswhistle :growth :helping-hand :hidden-power :ingrain :leech-seed :light-screen :mega-drain :mimic :nature-power :protect :rest :return :safeguard :secret-power :sleep-talk :sludge-bomb :snore :solarbeam :substitute :sunny-day :swagger :swords-dance :synthesis :toxic :double-edge :mimic]
  :sunflora [:hyper-beam :petal-dance :pound :razor-leaf :double-edge :mimic])

(deflearnset :dp
  :sunkern [:absorb :attract :bullet-seed :captivate :curse :cut :double-team :earth-power :encore :endeavor :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :helping-hand :hidden-power :ingrain :leech-seed :light-screen :mega-drain :natural-gift :nature-power :protect :razor-leaf :rest :return :safeguard :secret-power :seed-bomb :sleep-talk :sludge-bomb :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :uproar :worry-seed :double-edge :mimic]
  :sunflora [:giga-impact :hyper-beam :leaf-storm :petal-dance :pound :double-edge :mimic])

(deflearnset :bw
  :sunkern [:absorb :after-you :attract :bide :curse :cut :double-edge :double-team :earth-power :encore :endeavor :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :helping-hand :hidden-power :ingrain :leech-seed :light-screen :mega-drain :morning-sun :natural-gift :nature-power :protect :razor-leaf :rest :return :round :safeguard :seed-bomb :sleep-talk :sludge-bomb :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :uproar :worry-seed :double-edge :mimic]
  :sunflora [:bullet-seed :giga-impact :hyper-beam :leaf-storm :petal-dance :pound :double-edge :mimic])

