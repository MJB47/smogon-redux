
(in-ns 'smogon.dex)


(defpokemon :croagunk
  :name "Croagunk"
  :introduced-in :dp
  :types [[:fighting :poison]]
  :egggroups [[:humanshape]]
  :abilities [[:anticipation :dry-skin :poison-touch]]
  :stats [[48 61 40 61 40 50]]
  :weight [23]
  :height [0.7])

(defpokemon :toxicroak
  :name "Toxicroak"
  :introduced-in :dp
  :types [[:fighting :poison]]
  :egggroups [[:humanshape]]
  :abilities [[:anticipation :dry-skin :poison-touch]]
  :stats [[83 106 65 86 65 85]]
  :weight [44.4]
  :height [1.3])

(deffamily (familychain :croagunk :toxicroak))

(deflearnset :dp
  :croagunk [:astonish :attract :bounce :brick-break :bulk-up :bullet-punch :captivate :counter :cross-chop :dark-pulse :dig :double-team :dynamicpunch :earthquake :embargo :endure :facade :faint-attack :fake-out :feint :flatter :fling :focus-blast :focus-punch :frustration :fury-cutter :gunk-shot :headbutt :helping-hand :hidden-power :ice-punch :icy-wind :knock-off :low-kick :me-first :meditate :mud-bomb :mud-slap :nasty-plot :natural-gift :payback :poison-jab :poison-sting :protect :pursuit :rain-dance :rest :return :revenge :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :secret-power :shadow-ball :sleep-talk :sludge-bomb :smellingsalt :snatch :snore :spite :strength :substitute :sucker-punch :sunny-day :super-fang :swagger :taunt :thief :thunderpunch :torment :toxic :vacuum-wave :wake-up-slap :x-scissor]
  :toxicroak [:cut :giga-impact :hyper-beam :stone-edge :swords-dance])

(deflearnset :bw
  :croagunk [:acupressure :astonish :attract :bounce :brick-break :bulk-up :bulldoze :bullet-punch :counter :cross-chop :dark-pulse :dig :double-team :drain-punch :dual-chop :dynamicpunch :earthquake :embargo :facade :faint-attack :fake-out :feint :flatter :fling :focus-blast :foul-play :frustration :gunk-shot :headbutt :helping-hand :hidden-power :ice-punch :icy-wind :knock-off :low-kick :low-sweep :me-first :meditate :mud-bomb :mud-slap :nasty-plot :payback :poison-jab :poison-sting :protect :pursuit :rain-dance :rest :retaliate :return :revenge :rock-slide :rock-smash :rock-tomb :role-play :round :shadow-ball :sleep-talk :sludge-bomb :sludge-wave :smellingsalt :snatch :snore :spite :strength :substitute :sucker-punch :sunny-day :super-fang :swagger :taunt :thief :thunderpunch :torment :toxic :vacuum-wave :venoshock :wake-up-slap :work-up :x-scissor]
  :toxicroak [:cut :giga-impact :hyper-beam :stone-edge :swords-dance])
