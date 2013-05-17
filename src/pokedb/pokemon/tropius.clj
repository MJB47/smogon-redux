
(in-ns 'smogon.dex)


(defpokemon :tropius
  :name "Tropius"
  :introduced-in :rs
  :types [[:flying :grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :solar-power]
              :bw [:chlorophyll :harvest :solar-power]]
  :stats [[99 68 83 72 87 51]]
  :weight [100]
  :height [2])

(deffamily (familychain :tropius))

(deflearnset :rs
  :tropius [:aerial-ace :attract :body-slam :bullet-seed :cut :double-edge :double-team :earthquake :endure :facade :flash :fly :frustration :fury-cutter :giga-drain :growth :gust :headbutt :hidden-power :hyper-beam :leech-seed :leer :magical-leaf :mimic :mud-slap :nature-power :protect :razor-leaf :razor-wind :rest :return :roar :rock-smash :safeguard :secret-power :slam :sleep-talk :snore :solarbeam :steel-wing :stomp :strength :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :whirlwind :double-edge])

(deflearnset :dp
  :tropius [:aerial-ace :air-cutter :air-slash :attract :body-slam :bullet-seed :captivate :curse :cut :defog :double-team :dragon-dance :earthquake :endure :energy-ball :facade :flash :fly :frustration :fury-cutter :giga-drain :giga-impact :grass-knot :growth :gust :headbutt :hidden-power :hyper-beam :leaf-blade :leaf-storm :leech-seed :leer :magical-leaf :mud-slap :natural-gift :nature-power :ominous-wind :outrage :protect :razor-leaf :razor-wind :rest :return :roar :rock-smash :roost :safeguard :secret-power :silver-wind :slam :sleep-talk :snore :solarbeam :steel-wing :stomp :strength :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tailwind :toxic :twister :whirlwind :worry-seed :double-edge])

(deflearnset :bw
  :tropius [:aerial-ace :air-slash :attract :bestow :body-slam :bulldoze :bullet-seed :curse :cut :double-team :dragon-dance :dragon-pulse :earthquake :energy-ball :facade :flash :fly :frustration :giga-drain :giga-impact :grass-knot :growth :gust :headbutt :hidden-power :hyper-beam :leaf-blade :leaf-storm :leaf-tornado :leech-seed :leer :magical-leaf :natural-gift :nature-power :outrage :protect :razor-leaf :razor-wind :rest :return :roar :rock-smash :roost :round :safeguard :seed-bomb :slam :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tailwind :toxic :whirlwind :worry-seed :double-edge])
