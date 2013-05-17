
(in-ns 'smogon.dex)


(defpokemon :hoppip
  :name "Hoppip"
  :introduced-in :gs
  :types [[:flying :grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :leaf-guard]
              :bw [:chlorophyll :infiltrator :leaf-guard]]
  :stats [[35 35 40 35 55 50]]
  :weight [0.5]
  :height [0.4])

(defpokemon :skiploom
  :name "Skiploom"
  :introduced-in :gs
  :types [[:flying :grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :leaf-guard]
              :bw [:chlorophyll :infiltrator :leaf-guard]]
  :stats [[55 45 50 45 65 80]]
  :weight [1]
  :height [0.6])

(defpokemon :jumpluff
  :name "Jumpluff"
  :introduced-in :gs
  :types [[:flying :grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :leaf-guard]
              :bw [:chlorophyll :infiltrator :leaf-guard]]
  :stats [[75 55 70 55 85 110]]
  :weight [3]
  :height [0.8])

(deffamily (familychain :hoppip :skiploom :jumpluff))

(deflearnset :gs
  :hoppip [:amnesia :attract :confusion :cotton-spore :curse :defense-curl :double-edge :double-team :encore :endure :flash :frustration :giga-drain :growl :headbutt :hidden-power :leech-seed :mega-drain :pay-day :poisonpowder :protect :reflect :rest :return :sleep-powder :sleep-talk :snore :solarbeam :splash :stun-spore :sunny-day :swagger :sweet-scent :synthesis :tackle :tail-whip :toxic]
  :skiploom []
  :jumpluff [:hyper-beam])

(deflearnset :rs
  :hoppip [:aerial-ace :amnesia :attract :bullet-seed :confusion :cotton-spore :defense-curl :double-edge :double-team :encore :endure :facade :flash :frustration :giga-drain :helping-hand :hidden-power :leech-seed :mega-drain :mimic :poisonpowder :protect :psych-up :reflect :rest :return :secret-power :sleep-powder :sleep-talk :snore :solarbeam :splash :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :tail-whip :toxic :mimic]
  :skiploom [ :mimic]
  :jumpluff [:hyper-beam :mimic])

(deflearnset :dp
  :hoppip [:aerial-ace :amnesia :aromatherapy :attract :bounce :bullet-seed :captivate :confusion :cotton-spore :double-edge :double-team :encore :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :headbutt :helping-hand :hidden-power :leech-seed :mega-drain :memento :natural-gift :poisonpowder :protect :psych-up :reflect :rest :return :secret-power :seed-bomb :silver-wind :sleep-powder :sleep-talk :snore :solarbeam :splash :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :tail-whip :toxic :u-turn :worry-seed :mimic]
  :skiploom [ :mimic]
  :jumpluff [:giga-impact :hyper-beam :mimic])

(deflearnset :bw
  :hoppip [:acrobatics :aerial-ace :amnesia :aromatherapy :attract :bounce :bullet-seed :confusion :cotton-guard :cotton-spore :double-edge :double-team :encore :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :helping-hand :hidden-power :leech-seed :mega-drain :memento :poisonpowder :protect :psych-up :rage-powder :reflect :rest :return :round :seed-bomb :sleep-powder :sleep-talk :snore :solarbeam :splash :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :tail-whip :toxic :u-turn :worry-seed :mimic]
  :skiploom [ :mimic]
  :jumpluff [:giga-impact :hyper-beam :mimic])
