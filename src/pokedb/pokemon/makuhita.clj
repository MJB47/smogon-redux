
(in-ns 'smogon.dex)


(defpokemon :makuhita
  :name "Makuhita"
  :introduced-in :rs
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :thick-fat]
              :bw [:guts :sheer-force :thick-fat]]
  :stats [[72 60 30 20 30 25]]
  :weight [86.4]
  :height [1])

(defpokemon :hariyama
  :name "Hariyama"
  :introduced-in :rs
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :thick-fat]
              :bw [:guts :sheer-force :thick-fat]]
  :stats [[144 120 60 40 60 50]]
  :weight [253.8]
  :height [2.3])

(deffamily (familychain :makuhita :hariyama))

(deflearnset :rs
  :makuhita [:arm-thrust :attract :belly-drum :body-slam :brick-break :bulk-up :counter :cross-chop :detect :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :faint-attack :fake-out :fire-punch :focus-energy :focus-punch :foresight :frustration :helping-hand :hidden-power :ice-punch :knock-off :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :rain-dance :rest :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :sand-attack :secret-power :seismic-toss :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :surf :swagger :tackle :thunderpunch :toxic :vital-throw :whirlwind :body-slam :double-edge :mimic :refresh]
  :hariyama [:hyper-beam :body-slam :double-edge :mimic])

(deflearnset :dp
  :makuhita [:arm-thrust :attract :belly-drum :brick-break :bulk-up :bullet-punch :captivate :close-combat :counter :cross-chop :detect :dig :double-team :dynamicpunch :earthquake :endure :facade :faint-attack :fake-out :feint :fire-punch :fling :focus-blast :focus-energy :focus-punch :force-palm :foresight :frustration :headbutt :helping-hand :hidden-power :ice-punch :knock-off :low-kick :mud-slap :natural-gift :poison-jab :protect :rain-dance :rest :return :revenge :reversal :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :sand-attack :secret-power :seismic-toss :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :superpower :surf :swagger :tackle :thunderpunch :toxic :vacuum-wave :vital-throw :wake-up-slap :whirlpool :whirlwind :body-slam :double-edge :mimic :refresh]
  :hariyama [:brine :giga-impact :hyper-beam :iron-head :payback :stone-edge :body-slam :double-edge :mimic])

(deflearnset :bw
  :makuhita [:arm-thrust :attract :belly-drum :brick-break :bulk-up :bulldoze :bullet-punch :chip-away :close-combat :counter :cross-chop :detect :dig :double-team :dynamicpunch :earthquake :endure :facade :faint-attack :fake-out :feint :fire-punch :fling :focus-blast :focus-energy :focus-punch :force-palm :foresight :frustration :heavy-slam :helping-hand :hidden-power :ice-punch :knock-off :low-kick :low-sweep :poison-jab :protect :rain-dance :rest :retaliate :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :role-play :round :sand-attack :seismic-toss :sleep-talk :smack-down :smellingsalt :snore :strength :substitute :sunny-day :superpower :surf :swagger :tackle :thunderpunch :toxic :vital-throw :wake-up-slap :whirlwind :wide-guard :work-up :body-slam :double-edge :mimic :refresh]
  :hariyama [:brine :giga-impact :hyper-beam :iron-head :payback :stone-edge :body-slam :double-edge :mimic])
