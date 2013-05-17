
(in-ns 'smogon.dex)

(deffamily (familychain :bulbasaur :ivysaur :venusaur))

(defpokemon :bulbasaur
  :name "Bulbasaur"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:chlorophyll :overgrow]]
  :stats [[45 49 49 65 45]
          :gs [45 49 49 65 65 45]]
  :weight [6.9]
  :height [0.7])

(defpokemon :venusaur
  :name "Venusaur"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:chlorophyll :overgrow]]
  :stats [[80 82 83 100 80]
          :gs [80 82 83 100 100 80]]
  :weight [100]
  :height [2])

(defpokemon :ivysaur
  :name "Ivysaur"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:chlorophyll :overgrow]]
  :stats [[60 62 63 80 60]
          :gs [60 62 63 80 80 60]]
  :weight [13]
  :height [1])

(use 'smogon.dex)

(deflearnset :rb
  :bulbasaur [:bide :body-slam :cut :double-edge :double-team :growl :growth :leech-seed :mega-drain :mimic :poisonpowder :rage :razor-leaf :reflect :rest :sleep-powder :solarbeam :substitute :swords-dance :tackle :take-down :toxic :vine-whip :ancientpower :rock-throw]
  :venusaur [:hyper-beam])

(deflearnset :gs
  :bulbasaur [:attract :charm :curse :cut :defense-curl :double-team :endure :flash :frustration :fury-cutter :giga-drain :growl :growth :headbutt :hidden-power :leech-seed :light-screen :mud-slap :petal-dance :poisonpowder :protect :razor-leaf :razor-wind :rest :return :safeguard :skull-bash :sleep-powder :sleep-talk :snore :solarbeam :sunny-day :swagger :sweet-scent :synthesis :tackle :toxic :vine-whip :ancientpower :rock-throw]
  :venusaur [:hyper-beam :roar])

(deflearnset :rs
  :bulbasaur [:attract :body-slam :bullet-seed :charm :curse :cut :defense-curl :double-edge :double-team :endure :facade :flash :frustration :fury-cutter :giga-drain :grasswhistle :growl :growth :hidden-power :leech-seed :light-screen :magical-leaf :mimic :mud-slap :petal-dance :poisonpowder :protect :razor-leaf :rest :return :rock-smash :safeguard :secret-power :skull-bash :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :strength :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :toxic :vine-whip]
  :venusaur [:body-slam :earthquake :frenzy-plant :hyper-beam :mimic :roar :body-slam :mimic])

(deflearnset :dp
  :bulbasaur [:amnesia :attract :bullet-seed :captivate :charm :curse :cut :double-edge :double-team :endure :energy-ball :facade :flash :frustration :fury-cutter :giga-drain :grass-knot :grasswhistle :growl :growth :headbutt :hidden-power :ingrain :knock-off :leaf-storm :leech-seed :light-screen :magical-leaf :mud-slap :natural-gift :nature-power :petal-dance :poisonpowder :power-whip :protect :razor-leaf :rest :return :rock-smash :safeguard :secret-power :seed-bomb :skull-bash :sleep-powder :sleep-talk :sludge :sludge-bomb :snore :solarbeam :strength :string-shot :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :take-down :toxic :vine-whip :worry-seed]
  :venusaur [:block :earthquake :frenzy-plant :giga-impact :hyper-beam :outrage :roar :rock-climb :body-slam :mimic])

(deflearnset :bw
  :bulbasaur [:amnesia :attract :bind :charm :curse :cut :double-edge :double-team :echoed-voice :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grass-pledge :grasswhistle :growl :growth :hidden-power :ingrain :knock-off :leaf-storm :leech-seed :light-screen :magical-leaf :nature-power :petal-dance :poisonpowder :power-whip :protect :razor-leaf :rest :return :rock-smash :round :safeguard :seed-bomb :skull-bash :sleep-powder :sleep-talk :sludge :sludge-bomb :snore :solarbeam :strength :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :take-down :toxic :venoshock :vine-whip :worry-seed :block :weather-ball :false-swipe]
  :venusaur [:bulldoze :earthquake :frenzy-plant :giga-impact :hyper-beam :outrage :roar :body-slam :mimic])
