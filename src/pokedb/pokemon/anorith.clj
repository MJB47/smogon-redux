
(in-ns 'smogon.dex)

(defpokemon :anorith
  :name "Anorith"
  :introduced-in :rs
  :types [[:bug :rock]]
  :egggroups [[:water-3]]
  :abilities [[:battle-armor]
              :bw [:battle-armor :swift-swim]]
  :stats [[45 95 50 40 50 75]]
  :weight [12.5]
  :height [0.7])

(defpokemon :armaldo
  :name "Armaldo"
  :introduced-in :rs
  :types [[:bug :rock]]
  :egggroups [[:water-3]]
  :abilities [[:battle-armor]
              :bw [:battle-armor :swift-swim]]
  :stats [[75 125 100 70 80 45]]
  :weight [68.2]
  :height [1.5])

(deffamily (familychain :anorith :armaldo))

(deflearnset :rs
  :anorith [:aerial-ace :ancientpower :attract :body-slam :brick-break :cut :dig :double-edge :double-team :endure :facade :frustration :fury-cutter :harden :hidden-power :knock-off :metal-claw :mimic :mud-slap :mud-sport :protect :rapid-spin :rest :return :rock-blast :rock-slide :rock-smash :rock-tomb :sandstorm :scratch :secret-power :slash :sleep-talk :snore :substitute :sunny-day :swagger :swords-dance :toxic :water-gun :water-pulse]
  :armaldo [:earthquake :hyper-beam :iron-tail :seismic-toss :strength])

(deflearnset :dp
  :anorith [:aerial-ace :ancientpower :attract :brick-break :captivate :cross-poison :crush-claw :curse :cut :dig :double-team :earth-power :endure :facade :false-swipe :frustration :fury-cutter :harden :headbutt :hidden-power :iron-defense :knock-off :metal-claw :mud-slap :mud-sport :natural-gift :protect :rapid-spin :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :sand-attack :sandstorm :scratch :screech :secret-power :slash :sleep-talk :snore :stealth-rock :string-shot :substitute :sunny-day :swagger :swords-dance :toxic :water-gun :water-pulse :x-scissor]
  :armaldo [:aqua-tail :block :earthquake :flash-cannon :giga-impact :hyper-beam :iron-tail :low-kick :stone-edge :strength :superpower])

(deflearnset :bw
  :anorith [:aerial-ace :ancientpower :attract :body-slam :brick-break :bug-bite :cross-poison :crush-claw :curse :cut :dig :double-edge :double-team :earth-power :facade :false-swipe :frustration :fury-cutter :harden :hidden-power :hone-claws :iron-defense :knock-off :metal-claw :mimic :mud-sport :protect :rapid-spin :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :round :sand-attack :sandstorm :scratch :screech :slash :sleep-talk :smack-down :snore :stealth-rock :struggle-bug :substitute :sunny-day :swagger :swords-dance :toxic :water-gun :water-pulse :x-scissor]
  :armaldo [:aqua-tail :block :bulldoze :earthquake :flash-cannon :giga-impact :hyper-beam :iron-tail :low-kick :seismic-toss :stone-edge :strength :superpower])
