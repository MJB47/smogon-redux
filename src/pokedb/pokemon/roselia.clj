
(in-ns 'smogon.dex)


(defpokemon :budew
  :name "Budew"
  :introduced-in :dp
  :types [[:grass :poison]]
  :egggroups [[]]
  :abilities [[:leaf-guard :natural-cure :poison-point]]
  :stats [[40 30 35 50 70 55]]
  :weight [1.2]
  :height [0.2])

(defpokemon :roselia
  :name "Roselia"
  :introduced-in :rs
  :types [[:grass :poison]]
  :egggroups [[:fairy :plant]]
  :abilities [[:natural-cure :poison-point]
              :bw [:leaf-guard :natural-cure :poison-point]]
  :stats [[50 60 45 100 80 65]]
  :weight [2]
  :height [0.3])

(defpokemon :roserade
  :name "Roserade"
  :introduced-in :dp
  :types [[:grass :poison]]
  :egggroups [[:fairy :plant]]
  :abilities [[:natural-cure :poison-point :technician]]
  :stats [[60 70 55 125 105 90]]
  :weight [14.5]
  :height [0.9])

(deffamily (familychain :budew :roselia :roserade))

(deflearnset :rs
  :roselia [:absorb :aromatherapy :attract :body-slam :bullet-seed :cotton-spore :cut :double-edge :double-team :endure :facade :flash :frustration :fury-cutter :giga-drain :grasswhistle :growth :hidden-power :ingrain :leech-seed :magical-leaf :mega-drain :mimic :mud-slap :nightmare :petal-dance :pin-missile :poison-sting :protect :psych-up :rest :return :secret-power :shadow-ball :sleep-talk :sludge-bomb :snore :solarbeam :spikes :stun-spore :substitute :sunny-day :swagger :sweet-scent :swift :swords-dance :synthesis :toxic :double-edge :mimic :nightmare :sweet-kiss])

(deflearnset :dp
  :budew [:absorb :attract :bullet-seed :captivate :cotton-spore :cut :double-team :endure :energy-ball :extrasensory :facade :flash :frustration :giga-drain :grass-knot :growth :hidden-power :leaf-storm :mega-drain :mind-reader :mud-slap :natural-gift :pin-missile :protect :psych-up :rain-dance :razor-leaf :rest :return :secret-power :seed-bomb :shadow-ball :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :spikes :stun-spore :substitute :sunny-day :swagger :swift :swords-dance :synthesis :toxic :uproar :water-sport :worry-seed]
  :roselia [:aromatherapy :fury-cutter :grasswhistle :ingrain :leech-seed :magical-leaf :petal-dance :poison-jab :poison-sting :sweet-scent :toxic-spikes :double-edge :mimic :nightmare :sweet-kiss]
  :roserade [:giga-impact :hyper-beam :weather-ball])

(deflearnset :bw
  :budew [:absorb :attract :cotton-spore :covet :cut :double-team :energy-ball :extrasensory :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :hidden-power :leaf-storm :mega-drain :mind-reader :natural-gift :pin-missile :protect :psych-up :rain-dance :razor-leaf :rest :return :round :seed-bomb :shadow-ball :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :spikes :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :toxic :uproar :venoshock :water-sport :worry-seed]
  :roselia [:aromatherapy :ingrain :leech-seed :magical-leaf :petal-dance :poison-jab :poison-sting :sweet-scent :toxic-spikes :double-edge :mimic :nightmare :sweet-kiss]
  :roserade [:giga-impact :hyper-beam :weather-ball])
