
(in-ns 'smogon.dex)


(defpokemon :snorunt
  :name "Snorunt"
  :introduced-in :rs
  :types [[:ice]]
  :egggroups [[:fairy :mineral]]
  :abilities [[:inner-focus]
              :dp [:ice-body :inner-focus]
              :bw [:ice-body :inner-focus :moody]]
  :stats [[50 50 50 50 50 50]]
  :weight [16.8]
  :height [0.7])

(defpokemon :glalie
  :name "Glalie"
  :introduced-in :rs
  :types [[:ice]]
  :egggroups [[:fairy :mineral]]
  :abilities [[:inner-focus]
              :dp [:ice-body :inner-focus]
              :bw [:ice-body :inner-focus :moody]]
  :stats [[80 80 80 80 80 80]]
  :weight [256.5]
  :height [1.5])

(defpokemon :froslass
  :name "Froslass"
  :introduced-in :dp
  :types [[:ghost :ice]]
  :egggroups [[:fairy :mineral]]
  :abilities [[:cursed-body :snow-cloak]]
  :stats [[70 80 70 80 70 110]]
  :weight [26.6]
  :height [1.3])

(deffamily (familychain :snorunt [:glalie :froslass]))



(deflearnset :rs
  :snorunt [:attract :bite :blizzard :block :body-slam :crunch :double-edge :double-team :endure :facade :flash :frustration :hail :headbutt :hidden-power :ice-beam :icy-wind :leer :light-screen :mimic :powder-snow :protect :rain-dance :rest :return :safeguard :secret-power :shadow-ball :sleep-talk :snore :spikes :substitute :swagger :toxic :water-pulse :body-slam :double-edge :mimic :sing :water-pulse]
  :glalie [:defense-curl :earthquake :explosion :hyper-beam :rollout :selfdestruct :sheer-cold :taunt :torment :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :dp
  :snorunt [:attract :avalanche :bide :bite :blizzard :block :captivate :crunch :disable :double-team :endure :facade :flash :frustration :hail :headbutt :hidden-power :ice-beam :ice-fang :ice-shard :icy-wind :leer :light-screen :natural-gift :powder-snow :protect :rain-dance :rest :return :rollout :safeguard :secret-power :shadow-ball :sleep-talk :snore :spikes :spite :substitute :swagger :toxic :water-pulse :weather-ball :body-slam :double-edge :mimic :sing :water-pulse]
  :froslass [:astonish :confuse-ray :destiny-bond :dream-eater :embargo :fling :giga-impact :hyper-beam :ice-punch :mud-slap :ominous-wind :pain-split :payback :psych-up :psychic :shock-wave :signal-beam :snatch :sucker-punch :taunt :thunder :thunder-wave :thunderbolt :torment :trick :wake-up-slap]
  :glalie [:dark-pulse :earthquake :explosion :gyro-ball :iron-head :sheer-cold :super-fang :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :bw
  :snorunt [:attract :avalanche :bide :bite :blizzard :block :crunch :disable :double-team :facade :fake-tears :flash :frost-breath :frustration :hail :headbutt :hex :hidden-power :ice-beam :ice-fang :ice-shard :icy-wind :leer :light-screen :powder-snow :protect :rain-dance :rest :return :rollout :round :safeguard :shadow-ball :sleep-talk :snore :spikes :spite :substitute :swagger :toxic :weather-ball :body-slam :double-edge :mimic :sing :water-pulse]
  :froslass [:astonish :captivate :confuse-ray :destiny-bond :dream-eater :embargo :fling :giga-impact :hyper-beam :ice-punch :ominous-wind :pain-split :payback :psych-up :psychic :signal-beam :snatch :taunt :telekinesis :thunder :thunder-wave :thunderbolt :torment :trick :wake-up-slap]
  :glalie [:bulldoze :dark-pulse :earthquake :explosion :gyro-ball :iron-head :sheer-cold :super-fang :body-slam :double-edge :mimic :selfdestruct])

