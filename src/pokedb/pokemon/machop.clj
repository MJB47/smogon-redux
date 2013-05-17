
(in-ns 'smogon.dex)

(defpokemon :machop
  :name "Machop"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts]
              :dp [:guts :no-guard]
              :bw [:guts :no-guard :steadfast]]
  :stats [[70 80 50 35 35]
          :gs [70 80 50 35 35 35]]
  :weight [19.5]
  :height [0.8])

(defpokemon :machoke
  :name "Machoke"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts]
              :dp [:guts :no-guard]
              :bw [:guts :no-guard :steadfast]]
  :stats [[80 100 70 50 45]
          :gs [80 100 70 50 60 45]]
  :weight [70.5]
  :height [1.5])

(defpokemon :machamp
  :name "Machamp"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts]
              :dp [:guts :no-guard]
              :bw [:guts :no-guard :steadfast]]
  :stats [[90 130 80 65 55]
          :gs [90 130 80 65 85 55]]
  :weight [130]
  :height [1.6])

(deffamily (familychain :machop :machoke :machamp))

(deflearnset :rb
  :machop [:bide :body-slam :counter :dig :double-edge :double-team :earthquake :fire-blast :fissure :focus-energy :karate-chop :leer :low-kick :mega-kick :mega-punch :metronome :mimic :rage :rest :rock-slide :seismic-toss :skull-bash :strength :submission :substitute :take-down :toxic :false-swipe :thrash]
  :machoke []
  :machamp [:hyper-beam])

(deflearnset :gs
  :machop [:attract :cross-chop :curse :detect :dig :double-team :dynamicpunch :earthquake :encore :endure :fire-blast :fire-punch :flamethrower :focus-energy :foresight :frustration :headbutt :hidden-power :ice-punch :karate-chop :leer :light-screen :low-kick :meditate :mud-slap :protect :rest :return :rock-smash :rolling-kick :scary-face :seismic-toss :sleep-talk :snore :strength :submission :sunny-day :swagger :thief :thunderpunch :toxic :vital-throw :false-swipe :thrash]
  :machoke []
  :machamp [:hyper-beam])

(deflearnset :rs
  :machop [:attract :body-slam :brick-break :bulk-up :counter :cross-chop :dig :double-edge :double-team :dynamicpunch :earthquake :encore :endure :facade :fire-blast :fire-punch :flamethrower :focus-energy :focus-punch :foresight :frustration :hidden-power :ice-punch :karate-chop :leer :light-screen :low-kick :meditate :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :rain-dance :rest :return :revenge :rock-slide :rock-smash :rock-tomb :rolling-kick :scary-face :secret-power :seismic-toss :sleep-talk :smellingsalt :snore :strength :submission :substitute :sunny-day :swagger :thief :thunderpunch :toxic :vital-throw :body-slam :double-edge :mimic]
  :machoke [ :body-slam :double-edge :mimic]
  :machamp [:hyper-beam :body-slam :double-edge :mimic])

(deflearnset :dp
  :machop [:attract :brick-break :bulk-up :bullet-punch :captivate :close-combat :counter :cross-chop :dig :double-team :dynamicpunch :earthquake :encore :endure :facade :fire-blast :fire-punch :flamethrower :fling :focus-blast :focus-energy :focus-punch :foresight :frustration :headbutt :helping-hand :hidden-power :ice-punch :karate-chop :leer :light-screen :low-kick :meditate :mud-slap :natural-gift :payback :poison-jab :power-trick :protect :rain-dance :rest :return :revenge :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :rolling-kick :scary-face :secret-power :seismic-toss :sleep-talk :smellingsalt :snore :strength :submission :substitute :sunny-day :superpower :swagger :thief :thunderpunch :toxic :vacuum-wave :vital-throw :wake-up-slap :body-slam :double-edge :mimic]
  :machoke [ :body-slam :double-edge :mimic]
  :machamp [:giga-impact :hyper-beam :stone-edge :body-slam :double-edge :mimic])

(deflearnset :bw
  :machop [:attract :brick-break :bulk-up :bulldoze :bullet-punch :close-combat :counter :cross-chop :dig :double-team :dual-chop :dynamicpunch :earthquake :encore :facade :fire-blast :fire-punch :flamethrower :fling :focus-blast :focus-energy :foresight :frustration :heavy-slam :helping-hand :hidden-power :ice-punch :incinerate :karate-chop :knock-off :leer :light-screen :low-kick :low-sweep :meditate :payback :poison-jab :power-trick :protect :rain-dance :rest :retaliate :return :revenge :rock-slide :rock-smash :rock-tomb :role-play :rolling-kick :round :scary-face :seismic-toss :sleep-talk :smack-down :smellingsalt :snore :strength :submission :substitute :sunny-day :superpower :swagger :thief :thunderpunch :tickle :toxic :vital-throw :wake-up-slap :work-up :body-slam :double-edge :mimic]
  :machoke [ :body-slam :double-edge :mimic]
  :machamp [:giga-impact :hyper-beam :stone-edge :wide-guard :body-slam :double-edge :mimic])
