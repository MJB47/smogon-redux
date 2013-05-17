
(in-ns 'smogon.dex)


(defpokemon :igglybuff
  :name "Igglybuff"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:cute-charm]
              :bw [:cute-charm :friend-guard]]
  :stats [[90 30 15 40 20 15]]
  :weight [1]
  :height [0.3])

(defpokemon :jigglypuff
  :name "Jigglypuff"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:cute-charm]
              :bw [:cute-charm :friend-guard]]
  :stats [[115 45 20 25 20]
          :gs [115 45 20 45 25 20]]
  :weight [5.5]
  :height [0.5])

(defpokemon :wigglytuff
  :name "Wigglytuff"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:cute-charm]
              :bw [:cute-charm :frisk]]
  :stats [[140 70 45 50 45]
          :gs [140 70 45 75 50 45]]
  :weight [12]
  :height [1])

(deffamily (familychain :igglybuff :jigglypuff :wigglytuff))

(deflearnset :rb
  :jigglypuff [:bide :blizzard :body-slam :bubblebeam :counter :defense-curl :disable :double-edge :double-team :doubleslap :fire-blast :flash :ice-beam :mega-kick :mega-punch :mimic :pound :psychic :psywave :rage :reflect :rest :seismic-toss :sing :skull-bash :solarbeam :strength :submission :substitute :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack :water-gun]
  :wigglytuff [:hyper-beam])

(deflearnset :gs
  :igglybuff [:attract :charm :curse :defense-curl :detect :double-team :dream-eater :endure :faint-attack :fire-blast :flamethrower :flash :frustration :headbutt :hidden-power :icy-wind :mud-slap :nightmare :perish-song :pound :present :protect :psych-up :psychic :rain-dance :rest :return :rollout :shadow-ball :sing :sleep-talk :snore :solarbeam :sunny-day :swagger :sweet-kiss :toxic :zap-cannon]
  :jigglypuff [:blizzard :body-slam :disable :double-edge :doubleslap :dynamicpunch :fire-punch :ice-beam :ice-punch :strength :thunder :thunderbolt :thunderpunch]
  :wigglytuff [:hyper-beam])

(deflearnset :rs
  :igglybuff [:attract :body-slam :charm :counter :defense-curl :dig :double-edge :double-team :dream-eater :endure :facade :faint-attack :fake-tears :fire-blast :flamethrower :flash :frustration :hidden-power :icy-wind :light-screen :mega-kick :mega-punch :mimic :mud-slap :nightmare :perish-song :pound :present :protect :psych-up :psychic :rain-dance :reflect :rest :return :rollout :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :sing :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :water-pulse :wish]
  :jigglypuff [:blizzard :brick-break :disable :doubleslap :dynamicpunch :fire-punch :focus-punch :hyper-voice :ice-beam :ice-punch :snatch :strength :thunder :thunderbolt :thunderpunch :icy-wind :nightmare :seismic-toss]
  :wigglytuff [:hyper-beam :icy-wind :nightmare :seismic-toss])

(deflearnset :dp
  :igglybuff [:attract :bounce :captivate :charm :copycat :covet :defense-curl :dig :double-team :dream-eater :endeavor :endure :facade :faint-attack :fake-tears :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :headbutt :heal-bell :helping-hand :hidden-power :icy-wind :last-resort :light-screen :magic-coat :mud-slap :natural-gift :pain-split :perish-song :pound :present :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :rollout :safeguard :secret-power :shadow-ball :shock-wave :sing :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :uproar :water-pulse :wish]
  :jigglypuff [:blizzard :body-slam :brick-break :charge-beam :disable :double-edge :doubleslap :drain-punch :fire-punch :focus-punch :gyro-ball :hyper-voice :ice-beam :ice-punch :knock-off :mimic :snatch :stealth-rock :strength :thunder :thunderbolt :thunderpunch :wake-up-slap :icy-wind :nightmare :seismic-toss]
  :wigglytuff [:focus-blast :giga-impact :hyper-beam :icy-wind :nightmare :seismic-toss])

(deflearnset :bw
  :igglybuff [:attract :bounce :captivate :charm :copycat :covet :defense-curl :dig :double-team :dream-eater :echoed-voice :endeavor :facade :faint-attack :fake-tears :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :heal-bell :helping-hand :hidden-power :hyper-voice :icy-wind :incinerate :last-resort :light-screen :magic-coat :pain-split :perish-song :pound :present :protect :psych-up :psychic :punishment :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :shadow-ball :sing :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :uproar :wild-charge :wish :work-up]
  :jigglypuff [:blizzard :body-slam :brick-break :charge-beam :disable :double-edge :doubleslap :drain-punch :fire-punch :gyro-ball :ice-beam :ice-punch :knock-off :mimic :retaliate :rollout :snatch :stealth-rock :strength :thunder :thunderbolt :thunderpunch :wake-up-slap :icy-wind :nightmare :seismic-toss]
  :wigglytuff [:focus-blast :giga-impact :hyper-beam :magic-room :icy-wind :nightmare :seismic-toss])
