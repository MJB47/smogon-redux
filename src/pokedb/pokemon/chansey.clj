
(in-ns 'smogon.dex)

(defpokemon :happiny
  :name "Happiny"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:friend-guard :natural-cure :serene-grace]]
  :stats [[100 5 5 15 65 30]]
  :weight [24.4]
  :height [0.6])

(defpokemon :chansey
  :name "Chansey"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:natural-cure :serene-grace]
              :bw [:healer :natural-cure :serene-grace]]
  :stats [[250 5 5 105 50]
          :gs [250 5 5 35 105 50]]
  :weight [34.6]
  :height [1.1])

(defpokemon :blissey
  :name "Blissey"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:natural-cure :serene-grace]
              :bw [:healer :natural-cure :serene-grace]]
  :stats [[255 10 10 75 135 55]]
  :weight [46.8]
  :height [1.5])

(deffamily (familychain :happiny :chansey :blissey))

(deflearnset :rb
  :chansey [:bide :blizzard :body-slam :bubblebeam :counter :defense-curl :double-edge :double-team :doubleslap :egg-bomb :fire-blast :flash :growl :hyper-beam :ice-beam :light-screen :mega-kick :mega-punch :metronome :mimic :minimize :pound :psychic :psywave :rage :reflect :rest :seismic-toss :sing :skull-bash :softboiled :solarbeam :strength :submission :substitute :tail-whip :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack :water-gun])

(deflearnset :gs
  :chansey [:attract :blizzard :curse :defense-curl :double-edge :double-team :doubleslap :dream-eater :dynamicpunch :egg-bomb :endure :fire-blast :flamethrower :flash :frustration :growl :headbutt :heal-bell :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :light-screen :metronome :minimize :mud-slap :pound :present :protect :psych-up :psychic :rain-dance :rest :return :rock-smash :rollout :sandstorm :shadow-ball :sing :sleep-talk :snore :softboiled :solarbeam :strength :sunny-day :swagger :tail-whip :thunder :thunderbolt :toxic :zap-cannon])

(deflearnset :rs
  :chansey [:aromatherapy :attract :blizzard :body-slam :brick-break :calm-mind :counter :defense-curl :double-edge :double-team :doubleslap :dream-eater :dynamicpunch :earthquake :egg-bomb :endure :facade :fire-blast :flamethrower :flash :focus-punch :frustration :growl :hail :heal-bell :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :light-screen :mega-kick :mega-punch :metronome :mimic :minimize :mud-slap :pound :present :protect :psych-up :psychic :rain-dance :refresh :rest :return :rock-smash :rock-tomb :rollout :safeguard :sandstorm :secret-power :seismic-toss :shadow-ball :shock-wave :sing :skill-swap :sleep-talk :snatch :snore :softboiled :solarbeam :strength :substitute :sunny-day :swagger :tail-whip :thunder :thunder-wave :thunderbolt :toxic :water-pulse :wish])

(deflearnset :dp
  :happiny [:aromatherapy :attract :captivate :charm :copycat :counter :double-team :drain-punch :dream-eater :endeavor :endure :facade :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :hail :headbutt :heal-bell :helping-hand :hidden-power :icy-wind :last-resort :light-screen :metronome :mud-bomb :mud-slap :natural-gift :pound :present :protect :psych-up :psychic :rain-dance :recycle :refresh :rest :return :rollout :safeguard :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :uproar :water-pulse :zen-headbutt]
  :chansey [:blizzard :body-slam :brick-break :calm-mind :charge-beam :defense-curl :double-edge :doubleslap :earthquake :egg-bomb :fire-punch :focus-punch :giga-impact :growl :healing-wish :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :mimic :minimize :rock-climb :rock-slide :rock-smash :rock-tomb :sandstorm :seismic-toss :sing :skill-swap :snatch :softboiled :stealth-rock :strength :tail-whip :thunder :thunderbolt :thunderpunch :wish]
  :blissey [:avalanche :block :focus-blast])

(deflearnset :bw
  :happiny [:aromatherapy :attract :charm :copycat :counter :covet :double-team :drain-punch :dream-eater :echoed-voice :endeavor :endure :facade :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :hail :heal-bell :helping-hand :hidden-power :hyper-voice :icy-wind :incinerate :last-resort :light-screen :metronome :mud-bomb :natural-gift :pound :present :protect :psych-up :psychic :rain-dance :recycle :refresh :rest :return :round :safeguard :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :uproar :work-up :zen-headbutt]
  :chansey [:bestow :blizzard :body-slam :brick-break :bulldoze :calm-mind :charge-beam :defense-curl :double-edge :doubleslap :earthquake :egg-bomb :fire-punch :giga-impact :growl :heal-pulse :healing-wish :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :mimic :minimize :retaliate :rock-slide :rock-smash :rock-tomb :sandstorm :seismic-toss :shock-wave :sing :skill-swap :snatch :softboiled :stealth-rock :strength :tail-whip :take-down :thunder :thunderbolt :thunderpunch :wild-charge :wish]
  :blissey [:block :focus-blast])
