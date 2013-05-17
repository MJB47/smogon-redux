
(in-ns 'smogon.dex)

(defpokemon :cleffa
  :name "Cleffa"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:cute-charm]
              :dp [:cute-charm :magic-guard]
              :bw [:cute-charm :friend-guard :magic-guard]]
  :stats [[50 25 28 45 55 15]]
  :weight [3]
  :height [0.3])

(defpokemon :clefairy
  :name "Clefairy"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:cute-charm]
              :dp [:cute-charm :magic-guard]
              :bw [:cute-charm :friend-guard :magic-guard]]
  :stats [[70 45 48 60 35]
          :gs [70 45 48 60 65 35]]
  :weight [7.5]
  :height [0.6])

(defpokemon :clefable
  :name "Clefable"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:cute-charm]
              :dp [:cute-charm :magic-guard]
              :bw [:cute-charm :magic-guard :unaware]]
  :stats [[95 70 73 85 60]
          :gs [95 70 73 85 90 60]]
  :weight [40]
  :height [1.3])

(deffamily (familychain :cleffa :clefairy :clefable))

(deflearnset :rb
  :clefairy [:bide :blizzard :body-slam :bubblebeam :counter :defense-curl :double-edge :double-team :doubleslap :fire-blast :flash :growl :ice-beam :light-screen :mega-kick :mega-punch :metronome :mimic :minimize :pound :psychic :psywave :rage :reflect :rest :seismic-toss :sing :skull-bash :solarbeam :strength :submission :substitute :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack :water-gun :dizzy-punch :scary-face :swift :petal-dance]
  :clefable [:hyper-beam])

(deflearnset :gs
  :cleffa [:amnesia :attract :belly-drum :charm :curse :defense-curl :detect :double-team :dream-eater :encore :endure :fire-blast :flamethrower :flash :frustration :headbutt :hidden-power :icy-wind :iron-tail :metronome :mimic :mud-slap :nightmare :pound :present :protect :psych-up :psychic :rain-dance :rest :return :rollout :shadow-ball :sing :sleep-talk :snore :solarbeam :splash :sunny-day :swagger :sweet-kiss :toxic :zap-cannon :dizzy-punch :scary-face :swift :petal-dance]
  :clefairy [:blizzard :doubleslap :dynamicpunch :fire-punch :growl :ice-beam :ice-punch :light-screen :minimize :moonlight :strength :thunder :thunderbolt :thunderpunch]
  :clefable [:hyper-beam])

(deflearnset :rs
  :cleffa [:amnesia :attract :belly-drum :body-slam :charm :counter :defense-curl :dig :double-edge :double-team :dream-eater :encore :endure :facade :fire-blast :flamethrower :flash :frustration :hidden-power :icy-wind :iron-tail :light-screen :magical-leaf :mega-kick :mega-punch :metronome :mimic :mud-slap :nightmare :pound :present :protect :psych-up :psychic :rain-dance :reflect :rest :return :rollout :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :sing :sleep-talk :snore :softboiled :solarbeam :splash :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :water-pulse :wish]
  :clefairy [:blizzard :brick-break :calm-mind :cosmic-power :doubleslap :dynamicpunch :fire-punch :focus-punch :follow-me :growl :ice-beam :ice-punch :meteor-mash :minimize :moonlight :shock-wave :snatch :strength :thunder :thunderbolt :thunderpunch]
  :clefable [:hyper-beam])

(deflearnset :dp
  :cleffa [:amnesia :aromatherapy :attract :belly-drum :captivate :charm :copycat :covet :dig :double-team :dream-eater :encore :endeavor :endure :facade :fake-tears :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :headbutt :helping-hand :hidden-power :icy-wind :iron-tail :last-resort :light-screen :magic-coat :magical-leaf :metronome :mimic :mud-slap :natural-gift :pound :present :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :rollout :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :sing :sleep-talk :snore :solarbeam :splash :substitute :sunny-day :swagger :sweet-kiss :thunder-wave :toxic :trick :uproar :water-pulse :wish :zen-headbutt]
  :clefairy [:body-slam :blizzard :bounce :brick-break :calm-mind :charge-beam :cosmic-power :defense-curl :doubleslap :double-edge :drain-punch :fire-punch :focus-punch :follow-me :growl :heal-bell :healing-wish :ice-beam :ice-punch :knock-off :lucky-chant :meteor-mash :minimize :moonlight :nightmare :rock-smash :seismic-toss :shock-wave :snatch :stealth-rock :strength :thunder :thunderbolt :thunderpunch :wake-up-slap]
  :clefable [:focus-blast :giga-impact :hyper-beam])

(deflearnset :bw
  :cleffa [:after-you :amnesia :aromatherapy :attract :belly-drum :charm :copycat :covet :dig :double-team :dream-eater :echoed-voice :encore :endeavor :facade :fake-tears :fire-blast :flamethrower :flash :fling :frustration :grass-knot :gravity :helping-hand :hidden-power :hyper-voice :icy-wind :incinerate :iron-tail :last-resort :light-screen :magic-coat :magical-leaf :metronome :mimic :pound :present :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :shadow-ball :signal-beam :sing :sleep-talk :snore :solarbeam :splash :stored-power :substitute :sunny-day :swagger :sweet-kiss :telekinesis :thunder-wave :tickle :toxic :trick :uproar :wish :wonder-room :work-up :zen-headbutt]
  :clefairy [:bestow :blizzard :body-slam :bounce :brick-break :calm-mind :charge-beam :cosmic-power :defense-curl :doubleslap :double-edge :drain-punch :fire-punch :follow-me :growl :heal-bell :healing-wish :ice-beam :ice-punch :knock-off :lucky-chant :meteor-mash :minimize :moonlight :nightmare :retaliate :rock-smash :seismic-toss :shock-wave :snatch :stealth-rock :strength :thunder :thunderbolt :thunderpunch :wake-up-slap]
  :clefable [:focus-blast :giga-impact :hyper-beam])
