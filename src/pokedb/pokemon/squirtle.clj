
(in-ns 'smogon.dex)

(defpokemon :squirtle
  :name "Squirtle"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:rain-dish :torrent]]
  :stats [[44 48 65 50 43]
          :gs [44 48 65 50 64 43]]
  :weight [9]
  :height [0.5])

(defpokemon :wartortle
  :name "Wartortle"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:rain-dish :torrent]]
  :stats [[59 63 80 65 58]
          :gs [59 63 80 65 80 58]]
  :weight [22.5]
  :height [1])

(defpokemon :blastoise
  :name "Blastoise"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:rain-dish :torrent]]
  :stats [[79 83 100 85 78]
          :gs [79 83 100 85 105 78]]
  :weight [85.5]
  :height [1.6])

(deffamily (familychain :squirtle :wartortle :blastoise))



(deflearnset :rb
  :squirtle [:bide :bite :blizzard :body-slam :bubble :bubblebeam :counter :dig :double-edge :double-team :hydro-pump :ice-beam :mega-kick :mega-punch :mimic :rage :reflect :rest :seismic-toss :skull-bash :strength :submission :substitute :surf :tackle :tail-whip :take-down :toxic :water-gun :withdraw :zap-cannon]
  :wartortle []
  :blastoise [:earthquake :fissure :hyper-beam])

(deflearnset :gs
  :squirtle [:attract :bite :blizzard :bubble :confusion :curse :defense-curl :dig :double-team :dynamicpunch :endure :flail :foresight :frustration :haze :headbutt :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :mirror-coat :mist :mud-slap :protect :rain-dance :rapid-spin :rest :return :rock-smash :rollout :skull-bash :sleep-talk :snore :strength :surf :swagger :tackle :tail-whip :toxic :water-gun :waterfall :whirlpool :withdraw :zap-cannon]
  :wartortle []
  :blastoise [:earthquake :hyper-beam :roar])

(deflearnset :rs
  :squirtle [:attract :bite :blizzard :body-slam :brick-break :bubble :counter :defense-curl :dig :dive :double-edge :double-team :dynamicpunch :endure :facade :flail :focus-punch :foresight :frustration :hail :haze :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :mega-kick :mega-punch :mimic :mirror-coat :mist :mud-slap :mud-sport :protect :rain-dance :rapid-spin :refresh :rest :return :rock-smash :rollout :secret-power :seismic-toss :skull-bash :sleep-talk :snore :strength :substitute :surf :swagger :tackle :tail-whip :toxic :water-gun :water-pulse :waterfall :withdraw :yawn :body-slam :double-edge :mimic :seismic-toss]
  :wartortle [ :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :blastoise [:earthquake :hydro-cannon :hyper-beam :roar])

(deflearnset :dp
  :squirtle [:aqua-jet :aqua-ring :aqua-tail :attract :bite :blizzard :brick-break :brine :bubble :captivate :dig :dive :double-team :endure :facade :fake-out :flail :fling :focus-punch :foresight :frustration :gyro-ball :hail :haze :headbutt :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-defense :iron-tail :mirror-coat :mist :mud-slap :mud-sport :muddy-water :natural-gift :protect :rain-dance :rapid-spin :refresh :rest :return :rock-smash :rock-tomb :rollout :secret-power :skull-bash :sleep-talk :snore :strength :substitute :surf :swagger :tackle :tail-whip :toxic :water-gun :water-pulse :water-spout :waterfall :whirlpool :withdraw :yawn :zen-headbutt :body-slam :double-edge :mimic :seismic-toss]
  :wartortle [ :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :blastoise [:avalanche :earthquake :flash-cannon :focus-blast :giga-impact :hydro-cannon :hyper-beam :outrage :roar :rock-climb :rock-slide :signal-beam])

(deflearnset :bw
  :squirtle [:aqua-jet :aqua-ring :aqua-tail :attract :bite :blizzard :brick-break :brine :bubble :dig :dive :double-team :facade :fake-out :flail :fling :foresight :frustration :gyro-ball :hail :haze :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-defense :iron-tail :mirror-coat :mist :mud-sport :muddy-water :protect :rain-dance :rapid-spin :refresh :rest :return :rock-smash :rock-tomb :round :scald :skull-bash :sleep-talk :snore :strength :substitute :surf :swagger :tackle :tail-whip :toxic :water-gun :water-pledge :water-pulse :water-spout :waterfall :withdraw :yawn :zen-headbutt :block :false-swipe :follow-me :body-slam :double-edge :mimic :seismic-toss]
  :wartortle [ :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :blastoise [:bulldoze :dragon-tail :earthquake :flash-cannon :focus-blast :giga-impact :hydro-cannon :hyper-beam :outrage :roar :rock-slide :signal-beam :smack-down])

