
(in-ns 'smogon.dex)


(defpokemon :wooper
  :name "Wooper"
  :introduced-in :gs
  :types [[:ground :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :unaware :water-absorb]]
  :stats [[55 45 45 25 25 15]]
  :weight [8.5]
  :height [0.4])

(defpokemon :quagsire
  :name "Quagsire"
  :introduced-in :gs
  :types [[:ground :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :unaware :water-absorb]]
  :stats [[95 85 85 65 65 35]]
  :weight [75]
  :height [1.4])

(deffamily (familychain :wooper :quagsire))



(deflearnset :gs
  :wooper [:amnesia :ancientpower :attract :body-slam :curse :defense-curl :dig :double-team :dynamicpunch :earthquake :endure :flash :frustration :haze :headbutt :hidden-power :ice-beam :ice-punch :iron-tail :mist :mud-slap :protect :rain-dance :rest :return :rock-smash :rollout :safeguard :sandstorm :slam :sleep-talk :sludge-bomb :snore :surf :swagger :tail-whip :toxic :water-gun :whirlpool]
  :quagsire [:hyper-beam :strength])

(deflearnset :rs
  :wooper [:amnesia :ancientpower :attract :blizzard :body-slam :curse :defense-curl :dig :dive :double-edge :double-team :dynamicpunch :earthquake :endure :facade :flash :frustration :hail :haze :hidden-power :ice-beam :ice-punch :iron-tail :mimic :mist :mud-shot :mud-slap :mud-sport :protect :rain-dance :rest :return :rock-smash :rollout :safeguard :sandstorm :secret-power :slam :sleep-talk :sludge-bomb :snore :spit-up :stockpile :substitute :surf :swagger :swallow :tail-whip :toxic :water-gun :water-pulse :waterfall :yawn :double-edge :mimic]
  :quagsire [:brick-break :counter :focus-punch :hyper-beam :mega-kick :mega-punch :rock-tomb :seismic-toss :strength :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :wooper [:amnesia :ancientpower :aqua-tail :attract :blizzard :body-slam :captivate :counter :curse :dig :dive :double-kick :double-team :earth-power :earthquake :encore :endure :facade :flash :frustration :hail :haze :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :mist :mud-bomb :mud-shot :mud-slap :mud-sport :muddy-water :natural-gift :protect :rain-dance :recover :rest :return :rock-smash :rollout :safeguard :sandstorm :secret-power :slam :sleep-talk :sludge-bomb :snore :spit-up :stockpile :substitute :surf :swagger :swallow :tail-whip :toxic :water-gun :water-pulse :waterfall :whirlpool :yawn :double-edge :mimic]
  :quagsire [:brick-break :fling :focus-blast :focus-punch :giga-impact :hyper-beam :rock-slide :rock-tomb :stone-edge :strength :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :wooper [:acid-spray :after-you :amnesia :ancientpower :aqua-tail :attract :blizzard :body-slam :bulldoze :counter :curse :dig :dive :double-kick :double-team :earth-power :earthquake :encore :facade :flash :frustration :hail :haze :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :mist :mud-bomb :mud-shot :mud-sport :muddy-water :protect :rain-dance :recover :rest :return :rock-smash :round :safeguard :sandstorm :scald :slam :sleep-talk :sludge-bomb :sludge-wave :snore :spit-up :stockpile :substitute :surf :swagger :swallow :tail-whip :toxic :water-gun :waterfall :yawn :double-edge :mimic]
  :quagsire [:brick-break :fling :focus-blast :giga-impact :hyper-beam :rock-slide :rock-tomb :stone-edge :strength :thief :double-edge :mimic :seismic-toss])

