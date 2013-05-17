
(in-ns 'smogon.dex)


(defpokemon :tyrogue
  :name "Tyrogue"
  :introduced-in :gs
  :types [[:fighting]]
  :egggroups [[]]
  :abilities [[:guts]
              :dp [:guts :steadfast]
              :bw [:guts :steadfast :vital-spirit]]
  :stats [[35 35 35 35 35 35]]
  :weight [21]
  :height [0.7])

(defpokemon :hitmonchan
  :name "Hitmonchan"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:keen-eye]
              :dp [:iron-fist :keen-eye]
              :bw [:inner-focus :iron-fist :keen-eye]]
  :stats [[50 105 79 35 76]
          :gs [50 105 79 35 110 76]]
  :weight [50.2]
  :height [1.4])

(defpokemon :hitmonlee
  :name "Hitmonlee"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:limber]
              :dp [:limber :reckless]
              :bw [:limber :reckless :unburden]]
  :stats [[50 120 53 35 87]
          :gs [50 120 53 35 110 87]]
  :weight [49.8]
  :height [1.5])

(defpokemon :hitmontop
  :name "Hitmontop"
  :introduced-in :gs
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:intimidate]
              :dp [:intimidate :technician]
              :bw [:intimidate :steadfast :technician]]
  :stats [[50 95 95 35 110 70]]
  :weight [48]
  :height [1.4])

(deffamily (familychain :tyrogue [:hitmonlee :hitmonchan :hitmontop]))

(deflearnset :rb
  :hitmonchan [:agility :bide :body-slam :comet-punch :counter :double-edge :double-team :fire-punch :ice-punch :mega-kick :mega-punch :metronome :mimic :rage :rest :seismic-toss :skull-bash :strength :submission :substitute :swift :take-down :thunderpunch :toxic]
  :hitmonlee [:double-kick :focus-energy :hi-jump-kick :jump-kick :meditate :rolling-kick])

(deflearnset :gs
  :tyrogue [:attract :curse :detect :double-team :endure :frustration :headbutt :hi-jump-kick :hidden-power :mach-punch :mind-reader :mud-slap :protect :rapid-spin :rest :return :rock-smash :sleep-talk :snore :strength :sunny-day :swagger :swift :tackle :thief :toxic]
  :hitmonchan [:agility :comet-punch :counter :dynamicpunch :fire-punch :ice-punch :mega-punch :pursuit :thunderpunch]
  :hitmonlee [:double-kick :focus-energy :foresight :jump-kick :meditate :mega-kick :reversal :rolling-kick]
  :hitmontop [:dig :quick-attack :triple-kick])

(deflearnset :rs
  :tyrogue [:attract :body-slam :brick-break :bulk-up :counter :double-edge :double-team :earthquake :endure :facade :frustration :helping-hand :hi-jump-kick :hidden-power :mach-punch :mega-kick :mimic :mind-reader :mud-slap :protect :rain-dance :rapid-spin :rest :return :rock-slide :rock-smash :secret-power :seismic-toss :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :tackle :thief :toxic :body-slam :double-edge :mimic :seismic-toss]
  :hitmonchan [:agility :comet-punch :detect :dynamicpunch :fire-punch :focus-punch :ice-punch :mega-punch :metronome :pursuit :revenge :rock-tomb :sky-uppercut :thunderpunch :body-slam :double-edge :mimic :seismic-toss]
  :hitmonlee [:double-kick :focus-energy :foresight :jump-kick :meditate :reversal :rolling-kick :body-slam :double-edge :mimic :refresh :seismic-toss]
  :hitmontop [:dig :endeavor :quick-attack :sandstorm :triple-kick :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :tyrogue [:attract :brick-break :bulk-up :bullet-punch :captivate :counter :double-team :earthquake :endure :facade :fake-out :foresight :frustration :headbutt :helping-hand :hi-jump-kick :hidden-power :low-kick :mach-punch :mind-reader :mud-slap :natural-gift :protect :rain-dance :rapid-spin :rest :return :rock-slide :rock-smash :role-play :secret-power :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :tackle :thief :toxic :uproar :vacuum-wave :body-slam :double-edge :mimic :seismic-toss]
  :hitmonchan [:agility :close-combat :comet-punch :detect :drain-punch :feint :fire-punch :fling :focus-blast :focus-punch :ice-punch :mega-punch :pursuit :revenge :rock-climb :rock-tomb :sky-uppercut :stone-edge :thunderpunch :body-slam :double-edge :mimic :seismic-toss]
  :hitmonlee [:blaze-kick :bounce :double-kick :focus-energy :jump-kick :knock-off :meditate :mega-kick :poison-jab :reversal :rolling-kick :sucker-punch :superpower :body-slam :double-edge :mimic :refresh :seismic-toss]
  :hitmontop [:aerial-ace :dig :endeavor :gyro-ball :quick-attack :rollout :sandstorm :triple-kick :twister :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :tyrogue [:attract :brick-break :bulk-up :bulldoze :bullet-punch :counter :covet :double-team :earthquake :endure :facade :fake-out :feint :foresight :frustration :helping-hand :hi-jump-kick :hidden-power :low-kick :low-sweep :mach-punch :mind-reader :protect :pursuit :rain-dance :rapid-spin :rest :retaliate :return :rock-slide :rock-smash :role-play :round :sleep-talk :snore :strength :substitute :sunny-day :swagger :tackle :thief :toxic :uproar :vacuum-wave :work-up :body-slam :double-edge :mimic :seismic-toss]
  :hitmonchan [:agility :close-combat :comet-punch :detect :drain-punch :fire-punch :fling :focus-blast :focus-punch :ice-punch :mega-punch :quick-guard :revenge :rock-tomb :sky-uppercut :stone-edge :thunderpunch :body-slam :double-edge :mimic :seismic-toss]
  :hitmonlee [:blaze-kick :bounce :double-kick :focus-energy :jump-kick :knock-off :meditate :mega-kick :poison-jab :reversal :rolling-kick :superpower :wide-guard :body-slam :double-edge :mimic :refresh :seismic-toss]
  :hitmontop [:aerial-ace :dig :drill-run :endeavor :gyro-ball :quick-attack :sandstorm :triple-kick :body-slam :double-edge :mimic :seismic-toss])
