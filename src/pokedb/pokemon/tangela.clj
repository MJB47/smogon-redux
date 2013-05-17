
(in-ns 'smogon.dex)


(defpokemon :tangela
  :name "Tangela"
  :introduced-in :rb
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :dp [:chlorophyll :leaf-guard]
              :bw [:chlorophyll :leaf-guard :regenerator]]
  :stats [[65 55 115 100 60]
          :gs [65 55 115 100 40 60]]
  :weight [35]
  :height [1])

(defpokemon :tangrowth
  :name "Tangrowth"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll :leaf-guard :regenerator]]
  :stats [[100 100 125 110 50 50]]
  :weight [128.6]
  :height [2])

(deffamily (familychain :tangela :tangrowth))



(deflearnset :rb
  :tangela [:absorb :bide :bind :body-slam :constrict :cut :double-edge :double-team :growth :hyper-beam :mega-drain :mimic :poisonpowder :rage :rest :skull-bash :slam :sleep-powder :solarbeam :stun-spore :substitute :swords-dance :take-down :toxic :vine-whip])

(deflearnset :gs
  :tangela [:absorb :amnesia :attract :bind :confusion :constrict :curse :cut :double-team :endure :flail :flash :frustration :giga-drain :growth :headbutt :hidden-power :hyper-beam :mega-drain :poisonpowder :protect :psych-up :reflect :rest :return :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :sunny-day :swagger :sweet-scent :thief :toxic :vine-whip])

(deflearnset :rs
  :tangela [:absorb :amnesia :attract :bind :body-slam :bullet-seed :confusion :constrict :cut :double-edge :double-team :endure :facade :flail :flash :frustration :giga-drain :growth :hidden-power :hyper-beam :ingrain :leech-seed :mega-drain :mimic :nature-power :poisonpowder :protect :psych-up :reflect :rest :return :rock-smash :secret-power :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :swords-dance :thief :tickle :toxic :vine-whip :body-slam :double-edge :mimic :morning-sun])

(deflearnset :dp
  :tangela [:absorb :amnesia :ancientpower :attract :bind :bullet-seed :captivate :confusion :constrict :cut :double-team :endeavor :endure :energy-ball :facade :flail :flash :frustration :giga-drain :giga-impact :grass-knot :growth :headbutt :hidden-power :hyper-beam :ingrain :knock-off :leaf-storm :leech-seed :mega-drain :natural-gift :nature-power :pain-split :poisonpowder :power-swap :power-whip :protect :psych-up :reflect :rest :return :rock-smash :secret-power :seed-bomb :shock-wave :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :thief :tickle :toxic :vine-whip :worry-seed :wring-out :body-slam :double-edge :mimic :morning-sun]
  :tangrowth [:aerial-ace :block :brick-break :earthquake :fling :focus-blast :mud-slap :payback :poison-jab :rock-slide :rock-tomb :strength])

(deflearnset :bw
  :tangela [:absorb :amnesia :ancientpower :attract :bind :confusion :constrict :cut :double-team :endeavor :energy-ball :facade :flail :flash :frustration :giga-drain :giga-impact :grass-knot :growth :hidden-power :hyper-beam :ingrain :knock-off :leaf-storm :leech-seed :mega-drain :natural-gift :nature-power :pain-split :poisonpowder :power-swap :power-whip :protect :psych-up :rage-powder :reflect :rest :return :rock-smash :round :seed-bomb :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :thief :tickle :toxic :vine-whip :worry-seed :wring-out :body-slam :double-edge :mimic :morning-sun]
  :tangrowth [:aerial-ace :block :brick-break :bulldoze :earthquake :fling :focus-blast :payback :poison-jab :rock-slide :rock-tomb :strength])

