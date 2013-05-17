
(in-ns 'smogon.dex)

(defpokemon :geodude
  :name "Geodude"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rock-head :sand-veil :sturdy]]
  :stats [[40 80 100 30 20]
          :gs [40 80 100 30 30 20]]
  :weight [20]
  :height [0.4])

(defpokemon :graveler
  :name "Graveler"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rock-head :sand-veil :sturdy]]
  :stats [[55 95 115 45 35]
          :gs [55 95 115 45 45 35]]
  :weight [105]
  :height [1])

(defpokemon :golem
  :name "Golem"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rock-head :sand-veil :sturdy]]
  :stats [[80 110 130 55 45]
          :gs [80 110 130 55 65 45]]
  :weight [300]
  :height [1.4])

(deffamily (familychain :geodude :graveler :golem))

(deflearnset :rb
  :geodude [:bide :body-slam :counter :defense-curl :dig :double-edge :double-team :earthquake :explosion :fire-blast :fissure :harden :mega-punch :metronome :mimic :rage :rest :rock-slide :rock-throw :seismic-toss :selfdestruct :strength :submission :substitute :tackle :take-down :toxic :rapid-spin]
  :graveler []
  :golem [:hyper-beam :mega-kick])

(deflearnset :gs
  :geodude [:attract :curse :defense-curl :dig :double-team :dynamicpunch :earthquake :endure :explosion :fire-blast :fire-punch :flamethrower :frustration :harden :headbutt :hidden-power :magnitude :mega-punch :mud-slap :protect :rest :return :rock-slide :rock-smash :rock-throw :rollout :sandstorm :selfdestruct :sleep-talk :snore :strength :sunny-day :swagger :tackle :toxic :rapid-spin]
  :graveler []
  :golem [:fury-cutter :hyper-beam :roar])

(deflearnset :rs
  :geodude [:attract :block :body-slam :brick-break :counter :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :fire-blast :fire-punch :flamethrower :focus-punch :frustration :hidden-power :magnitude :mega-punch :metronome :mimic :mud-slap :mud-sport :protect :rest :return :rock-blast :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sandstorm :secret-power :seismic-toss :selfdestruct :sleep-talk :snore :strength :substitute :sunny-day :swagger :tackle :toxic :body-slam :double-edge :mimic :seismic-toss :selfdestruct]
  :graveler [ :body-slam :mimic :seismic-toss]
  :golem [:fury-cutter :hyper-beam :mega-kick :roar :body-slam :mimic :seismic-toss])

(deflearnset :dp
  :geodude [:ancientpower :attract :block :brick-break :captivate :curse :defense-curl :dig :double-edge :double-team :earth-power :earthquake :endure :explosion :facade :fire-blast :fire-punch :flail :flamethrower :fling :focus-punch :frustration :gyro-ball :hammer-arm :headbutt :hidden-power :magnitude :mega-punch :mud-slap :mud-sport :natural-gift :protect :rest :return :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sandstorm :secret-power :selfdestruct :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sucker-punch :sunny-day :superpower :swagger :tackle :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss :selfdestruct]
  :graveler [ :body-slam :mimic :seismic-toss]
  :golem [:focus-blast :fury-cutter :giga-impact :hyper-beam :iron-head :roar :body-slam :mimic :seismic-toss])

(deflearnset :bw
  :geodude [:attract :autotomize :block :brick-break :bulldoze :curse :defense-curl :dig :double-edge :double-team :earth-power :earthquake :endure :explosion :facade :fire-blast :fire-punch :flail :flamethrower :fling :focus-punch :frustration :gyro-ball :hammer-arm :hidden-power :incinerate :iron-defense :magnitude :mega-punch :mud-sport :protect :rest :return :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :sandstorm :selfdestruct :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :superpower :swagger :tackle :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss :selfdestruct]
  :graveler [ :body-slam :mimic :seismic-toss]
  :golem [:focus-blast :giga-impact :heavy-slam :hyper-beam :iron-head :roar :steamroller :body-slam :mimic :seismic-toss])
