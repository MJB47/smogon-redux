
(in-ns 'smogon.dex)


(defpokemon :whismur
  :name "Whismur"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground :monster]]
  :abilities [[:soundproof]
              :bw [:rattled :soundproof]]
  :stats [[64 51 23 51 23 28]]
  :weight [16.3]
  :height [0.6])

(defpokemon :loudred
  :name "Loudred"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground :monster]]
  :abilities [[:soundproof]
              :bw [:scrappy :soundproof]]
  :stats [[84 71 43 71 43 48]]
  :weight [40.5]
  :height [1])

(defpokemon :exploud
  :name "Exploud"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground :monster]]
  :abilities [[:soundproof]
              :bw [:scrappy :soundproof]]
  :stats [[104 91 63 91 63 68]]
  :weight [84]
  :height [1.5])

(deffamily (familychain :whismur :loudred :exploud))


(deflearnset :rs
  :whismur [:astonish :attract :blizzard :body-slam :counter :defense-curl :double-edge :double-team :dynamicpunch :endure :extrasensory :facade :fire-blast :fire-punch :flamethrower :frustration :hidden-power :howl :hyper-voice :ice-beam :ice-punch :icy-wind :mega-kick :mega-punch :mimic :mud-slap :pound :protect :psych-up :rain-dance :rest :return :roar :rollout :screech :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :smellingsalt :snore :solarbeam :stomp :substitute :sunny-day :supersonic :swagger :take-down :thunderpunch :toxic :uproar :water-pulse :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :loudred [:brick-break :earthquake :overheat :rock-slide :rock-smash :strength :taunt :torment :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :exploud [:hyper-beam :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :whismur [:astonish :attract :blizzard :captivate :double-team :endeavor :endure :extrasensory :facade :fire-blast :fire-punch :flamethrower :fling :frustration :hammer-arm :headbutt :hidden-power :howl :hyper-voice :ice-beam :ice-punch :icy-wind :mud-slap :natural-gift :pound :protect :rain-dance :rest :return :roar :rollout :screech :secret-power :shadow-ball :shock-wave :sleep-talk :smellingsalt :smokescreen :snore :solarbeam :stomp :substitute :sunny-day :supersonic :swagger :take-down :thunderpunch :toxic :uproar :water-pulse :zen-headbutt :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :loudred [:bite :brick-break :earthquake :low-kick :overheat :rock-slide :rock-smash :rock-tomb :strength :taunt :torment :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :exploud [:avalanche :crunch :fire-fang :focus-blast :giga-impact :hyper-beam :ice-fang :outrage :rock-climb :surf :thunder-fang :whirlpool :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :whismur [:astonish :attract :blizzard :circle-throw :double-team :echoed-voice :endeavor :extrasensory :facade :fake-tears :fire-blast :fire-punch :flamethrower :fling :frustration :hammer-arm :hidden-power :howl :hyper-voice :ice-beam :ice-punch :icy-wind :incinerate :pound :protect :rain-dance :rest :retaliate :return :roar :round :screech :shadow-ball :sleep-talk :smellingsalt :smokescreen :snore :solarbeam :stomp :substitute :sunny-day :supersonic :swagger :synchronoise :take-down :thunderpunch :toxic :uproar :work-up :zen-headbutt :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :loudred [:bite :brick-break :bulldoze :earthquake :low-kick :overheat :rock-slide :rock-smash :rock-tomb :smack-down :strength :taunt :torment :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :exploud [:crunch :fire-fang :focus-blast :giga-impact :hyper-beam :ice-fang :outrage :surf :thunder-fang :body-slam :double-edge :icy-wind :mimic :seismic-toss])

