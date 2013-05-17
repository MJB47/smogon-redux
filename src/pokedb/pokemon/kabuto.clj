
(in-ns 'smogon.dex)


(defpokemon :kabuto
  :name "Kabuto"
  :introduced-in :rb
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:battle-armor :swift-swim]
              :bw [:battle-armor :swift-swim :weak-armor]]
  :stats [[30 80 90 45 55]
          :gs [30 80 90 55 45 55]]
  :weight [11.5]
  :height [0.5])

(defpokemon :kabutops
  :name "Kabutops"
  :introduced-in :rb
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:battle-armor :swift-swim]
              :bw [:battle-armor :swift-swim :weak-armor]]
  :stats [[60 115 105 70 80]
          :gs [60 115 105 65 70 80]]
  :weight [40.5]
  :height [1.3])

(deffamily (familychain :kabuto :kabutops))

(deflearnset :rb
  :kabuto [:absorb :bide :blizzard :body-slam :bubblebeam :double-edge :double-team :harden :hydro-pump :ice-beam :leer :mimic :rage :reflect :rest :scratch :slash :substitute :surf :take-down :toxic :water-gun]
  :kabutops [:cut :hyper-beam :mega-kick :razor-wind :seismic-toss :skull-bash :submission :swords-dance])

(deflearnset :gs
  :kabuto [:absorb :ancientpower :attract :aurora-beam :blizzard :bubblebeam :curse :dig :double-team :endure :flail :frustration :giga-drain :harden :hidden-power :ice-beam :icy-wind :leer :mega-drain :protect :rain-dance :rapid-spin :rest :return :rock-smash :rollout :sand-attack :sandstorm :scratch :sleep-talk :snore :swagger :thief :toxic]
  :kabutops [:cut :fury-cutter :headbutt :hyper-beam :slash :surf :whirlpool])

(deflearnset :rs
  :kabuto [:absorb :aerial-ace :ancientpower :attract :aurora-beam :blizzard :body-slam :bubblebeam :confuse-ray :dig :double-edge :double-team :endure :facade :flail :frustration :giga-drain :hail :harden :hidden-power :ice-beam :icy-wind :knock-off :leer :mega-drain :metal-sound :mimic :mud-shot :protect :rain-dance :rapid-spin :rest :return :rock-slide :rock-smash :rock-tomb :rollout :sand-attack :sandstorm :scratch :secret-power :sleep-talk :snore :substitute :surf :swagger :thief :toxic :water-pulse :waterfall :body-slam :double-edge :mimic]
  :kabutops [:brick-break :cut :dive :fury-cutter :hyper-beam :mega-kick :seismic-toss :slash :swords-dance :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :kabuto [:absorb :ancientpower :aqua-jet :attract :aurora-beam :blizzard :brine :bubblebeam :captivate :confuse-ray :dig :double-team :earth-power :endure :facade :flail :frustration :giga-drain :hail :harden :hidden-power :ice-beam :icy-wind :iron-defense :knock-off :leer :mega-drain :metal-sound :mud-shot :mud-slap :natural-gift :protect :rain-dance :rapid-spin :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :sand-attack :sandstorm :scratch :screech :secret-power :sleep-talk :snore :stealth-rock :substitute :surf :swagger :thief :toxic :water-pulse :waterfall :whirlpool :wring-out :body-slam :double-edge :mimic]
  :kabutops [:aerial-ace :aqua-tail :brick-break :cut :dive :feint :fury-cutter :giga-impact :headbutt :hyper-beam :low-kick :night-slash :rock-climb :slash :stone-edge :superpower :swords-dance :x-scissor :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :kabuto [:absorb :ancientpower :aqua-jet :attract :aurora-beam :blizzard :bubblebeam :confuse-ray :dig :double-team :earth-power :endure :facade :flail :foresight :frustration :giga-drain :hail :harden :hidden-power :hone-claws :ice-beam :icy-wind :iron-defense :knock-off :leer :mega-drain :metal-sound :mud-shot :protect :rain-dance :rapid-spin :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :round :sand-attack :sandstorm :scald :scratch :screech :sleep-talk :smack-down :snore :stealth-rock :substitute :surf :swagger :thief :toxic :waterfall :wring-out :body-slam :double-edge :mimic]
  :kabutops [:aerial-ace :aqua-tail :brick-break :cut :dive :feint :giga-impact :hyper-beam :low-kick :night-slash :slash :stone-edge :superpower :swords-dance :x-scissor :body-slam :double-edge :mimic :seismic-toss])
