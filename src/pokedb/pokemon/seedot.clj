
(in-ns 'smogon.dex)


(defpokemon :seedot
  :name "Seedot"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:chlorophyll :early-bird]
              :bw [:chlorophyll :early-bird :pickpocket]]
  :stats [[40 40 50 30 30 30]]
  :weight [4]
  :height [0.5])

(defpokemon :nuzleaf
  :name "Nuzleaf"
  :introduced-in :rs
  :types [[:dark :grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:chlorophyll :early-bird]
              :bw [:chlorophyll :early-bird :pickpocket]]
  :stats [[70 70 40 60 40 60]]
  :weight [28]
  :height [1])

(defpokemon :shiftry
  :name "Shiftry"
  :introduced-in :rs
  :types [[:dark :grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:chlorophyll :early-bird]
              :bw [:chlorophyll :early-bird :pickpocket]]
  :stats [[90 100 60 90 60 80]]
  :weight [59.6]
  :height [1.3])

(deffamily (familychain :seedot :nuzleaf :shiftry))

(deflearnset :rs
  :seedot [:amnesia :attract :bide :body-slam :bullet-seed :defense-curl :dig :double-edge :double-team :endure :explosion :facade :false-swipe :flash :frustration :giga-drain :growth :harden :hidden-power :leech-seed :mimic :nature-power :protect :quick-attack :razor-wind :rest :return :rock-smash :rollout :secret-power :selfdestruct :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swords-dance :synthesis :take-down :toxic :body-slam :double-edge :giga-drain :mimic :refresh :secret-power :selfdestruct]
  :nuzleaf [:brick-break :cut :extrasensory :faint-attack :fake-out :fury-cutter :hyper-beam :mega-kick :mud-slap :pound :psych-up :rock-tomb :strength :swift :thief :torment :body-slam :double-edge :mimic :selfdestruct]
  :shiftry [:aerial-ace :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :dp
  :seedot [:amnesia :attract :bide :bullet-seed :captivate :dig :double-team :endure :energy-ball :explosion :facade :false-swipe :flash :frustration :giga-drain :grass-knot :growth :harden :headbutt :hidden-power :leech-seed :nasty-plot :natural-gift :nature-power :power-swap :protect :quick-attack :razor-wind :rest :return :rock-smash :rollout :secret-power :seed-bomb :shadow-ball :sleep-talk :snore :solarbeam :spite :substitute :sunny-day :swagger :swords-dance :synthesis :take-down :toxic :worry-seed :body-slam :double-edge :giga-drain :mimic :refresh :secret-power :selfdestruct]
  :nuzleaf [:brick-break :cut :dark-pulse :embargo :extrasensory :faint-attack :fake-out :fling :fury-cutter :hyper-beam :low-kick :mud-slap :payback :pound :psych-up :razor-leaf :rock-slide :rock-tomb :strength :swift :thief :torment :body-slam :double-edge :mimic :selfdestruct]
  :shiftry [:aerial-ace :air-cutter :bounce :defog :focus-blast :giga-impact :icy-wind :knock-off :leaf-storm :ominous-wind :silver-wind :sucker-punch :tailwind :twister :whirlwind :x-scissor :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :bw
  :seedot [:amnesia :attract :beat-up :bide :bullet-seed :defog :dig :double-team :energy-ball :explosion :facade :false-swipe :flash :foul-play :frustration :giga-drain :grass-knot :growth :harden :hidden-power :leech-seed :nasty-plot :nature-power :power-swap :protect :quick-attack :razor-wind :rest :retaliate :return :rock-smash :round :seed-bomb :shadow-ball :sleep-talk :snore :solarbeam :spite :substitute :sunny-day :swagger :swords-dance :synthesis :take-down :toxic :worry-seed :body-slam :double-edge :giga-drain :mimic :refresh :secret-power :selfdestruct]
  :nuzleaf [:brick-break :cut :dark-pulse :embargo :extrasensory :faint-attack :fake-out :fling :hyper-beam :low-kick :low-sweep :payback :pound :psych-up :razor-leaf :rock-slide :rock-tomb :snarl :strength :thief :torment :body-slam :double-edge :mimic :selfdestruct]
  :shiftry [:aerial-ace :bounce :focus-blast :giga-impact :icy-wind :knock-off :leaf-storm :leaf-tornado :tailwind :whirlwind :x-scissor :body-slam :double-edge :mimic :selfdestruct])
