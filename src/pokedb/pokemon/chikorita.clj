
(in-ns 'smogon.dex)


(defpokemon :chikorita
  :name "Chikorita"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:leaf-guard :overgrow]]
  :stats [[45 49 65 49 65 45]]
  :weight [6.4]
  :height [0.9])

(defpokemon :bayleef
  :name "Bayleef"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:leaf-guard :overgrow]]
  :stats [[60 62 80 63 80 60]]
  :weight [15.8]
  :height [1.2])

(defpokemon :meganium
  :name "Meganium"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow]
              :bw [:leaf-guard :overgrow]]
  :stats [[80 82 100 83 100 80]]
  :weight [100.5]
  :height [1.8])

(deffamily (familychain :chikorita :bayleef :meganium))

(deflearnset :gs
  :chikorita [:ancientpower :attract :body-slam :counter :curse :cut :detect :double-team :endure :flail :flash :frustration :giga-drain :growl :headbutt :hidden-power :iron-tail :leech-seed :light-screen :mud-slap :poisonpowder :protect :razor-leaf :reflect :rest :return :safeguard :sleep-talk :snore :solarbeam :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :toxic :vine-whip]
  :bayleef [:fury-cutter :rock-smash :strength]
  :meganium [:earthquake :hyper-beam])

(deflearnset :rs
  :chikorita [:ancientpower :attract :body-slam :bullet-seed :counter :cut :double-edge :double-team :endure :facade :flail :flash :frustration :giga-drain :grasswhistle :growl :hidden-power :ingrain :iron-tail :leech-seed :light-screen :mimic :mud-slap :nature-power :poisonpowder :protect :razor-leaf :reflect :rest :return :safeguard :secret-power :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :toxic :vine-whip]
  :bayleef [:fury-cutter :rock-smash :strength]
  :meganium [:earthquake :hyper-beam])

(deflearnset :dp
  :chikorita [:ancientpower :aromatherapy :attract :body-slam :bullet-seed :captivate :counter :cut :double-team :endure :energy-ball :facade :flail :flash :frustration :fury-cutter :giga-drain :grass-knot :grasswhistle :growl :headbutt :hidden-power :ingrain :iron-tail :leaf-storm :leech-seed :light-screen :magic-coat :magical-leaf :mud-slap :natural-gift :nature-power :poisonpowder :protect :razor-leaf :reflect :rest :return :safeguard :secret-power :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :toxic :vine-whip :worry-seed :wring-out]
  :bayleef [:rock-smash :strength]
  :meganium [:earthquake :frenzy-plant :giga-impact :hyper-beam :outrage :petal-dance :rock-climb])

(deflearnset :bw
  :chikorita [:ancientpower :aromatherapy :attract :body-slam :counter :cut :double-team :echoed-voice :energy-ball :facade :flail :flash :frustration :giga-drain :grass-knot :grass-pledge :grasswhistle :growl :heal-pulse :hidden-power :ingrain :iron-tail :leaf-storm :leech-seed :light-screen :magic-coat :magical-leaf :natural-gift :nature-power :poisonpowder :protect :razor-leaf :reflect :refresh :rest :return :round :safeguard :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :toxic :vine-whip :worry-seed :wring-out]
  :bayleef [:rock-smash :strength]
  :meganium [:bulldoze :dragon-tail :earthquake :frenzy-plant :giga-impact :hyper-beam :outrage :petal-dance])
