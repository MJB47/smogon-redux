
(in-ns 'smogon.dex)


(defpokemon :eevee
  :name "Eevee"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:run-away]
              :dp [:adaptability :run-away]
              :bw [:adaptability :anticipation :run-away]]
  :stats [[55 55 50 65 55]
          :gs [55 55 50 45 65 55]]
  :weight [6.5]
  :height [0.3])

(defpokemon :flareon
  :name "Flareon"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire]
              :bw [:flash-fire :guts]]
  :stats [[65 130 60 110 65]
          :gs [65 130 60 95 110 65]]
  :weight [25]
  :height [0.9])

(defpokemon :jolteon
  :name "Jolteon"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:volt-absorb]
              :bw [:quick-feet :volt-absorb]]
  :stats [[65 65 60 110 130]
          :gs [65 65 60 110 95 130]]
  :weight [24.5]
  :height [0.8])

(defpokemon :vaporeon
  :name "Vaporeon"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:water-absorb]
              :bw [:hydration :water-absorb]]
  :stats [[130 65 60 110 65]
          :gs [130 65 60 110 95 65]]
  :weight [29]
  :height [1])

(defpokemon :espeon
  :name "Espeon"
  :introduced-in :gs
  :types [[:psychic]]
  :egggroups [[:ground]]
  :abilities [[:synchronize]
              :bw [:magic-bounce :synchronize]]
  :stats [[65 65 60 130 95 110]]
  :weight [26.5]
  :height [0.9])

(defpokemon :umbreon
  :name "Umbreon"
  :introduced-in :gs
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:synchronize]
              :bw [:inner-focus :synchronize]]
  :stats [[95 65 110 60 130 65]]
  :weight [27]
  :height [1])

(defpokemon :leafeon
  :name "Leafeon"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground]]
  :abilities [[:chlorophyll :leaf-guard]]
  :stats [[65 110 130 60 65 95]]
  :weight [25.5]
  :height [1])

(defpokemon :glaceon
  :name "Glaceon"
  :introduced-in :dp
  :types [[:ice]]
  :egggroups [[:ground]]
  :abilities [[:snow-cloak]
              :bw [:ice-body :snow-cloak]]
  :stats [[65 60 110 130 95 65]]
  :weight [25.9]
  :height [0.8])

(deffamily
  (familyalts #(familychain :eevee %)
              :vaporeon
              :jolteon
              :flareon
              :espeon
              :umbreon
              :leafeon
              :glaceon))

(deflearnset :rb
  :eevee [:bide :bite :body-slam :double-edge :double-team :focus-energy :growl :mimic :quick-attack :rage :reflect :rest :sand-attack :skull-bash :substitute :swift :tackle :tail-whip :take-down :toxic]
  :flareon [:ember :fire-blast :fire-spin :flamethrower :hyper-beam :leer :smog]
  :jolteon [:agility :double-kick :flash :pin-missile :thunder :thunder-wave :thunderbolt :thundershock]
  :vaporeon [:acid-armor :aurora-beam :blizzard :bubblebeam :haze :hydro-pump :ice-beam :mist :surf :water-gun])

(deflearnset :gs
  :eevee [:attract :baton-pass :bite :charm :curse :detect :double-team :endure :flail :focus-energy :frustration :growl :headbutt :hidden-power :iron-tail :mud-slap :protect :quick-attack :rain-dance :rest :return :sand-attack :shadow-ball :sleep-talk :snore :sunny-day :swagger :swift :tackle :tail-whip :take-down :toxic]
  :espeon [:confusion :cut :dream-eater :flash :hyper-beam :morning-sun :nightmare :psybeam :psych-up :psychic :zap-cannon]
  :flareon [:ember :fire-blast :fire-spin :flamethrower :leer :roar :smog]
  :jolteon [:agility :double-kick :pin-missile :thunder :thunder-wave :thunderbolt :thundershock]
  :umbreon [:confuse-ray :faint-attack :mean-look :moonlight :pursuit :screech]
  :vaporeon [:acid-armor :aurora-beam :blizzard :haze :hydro-pump :ice-beam :icy-wind :surf :water-gun :waterfall :whirlpool])

(deflearnset :rs
  :eevee [:attract :baton-pass :bite :body-slam :charm :curse :dig :double-edge :double-team :endure :facade :flail :frustration :growl :helping-hand :hidden-power :iron-tail :mimic :mud-slap :protect :quick-attack :rain-dance :rest :return :sand-attack :secret-power :shadow-ball :sleep-talk :snore :substitute :sunny-day :swagger :swift :tackle :tail-whip :take-down :tickle :toxic :wish :body-slam :double-edge :mimic]
  :espeon [:calm-mind :confusion :cut :dream-eater :flash :hyper-beam :light-screen :morning-sun :nightmare :psybeam :psych-up :psychic :reflect :skill-swap :body-slam :double-edge :mimic :nightmare]
  :flareon [:ember :fire-blast :fire-spin :flamethrower :leer :overheat :roar :smog :body-slam :double-edge :mimic]
  :jolteon [:agility :double-kick :pin-missile :shock-wave :thunder :thunder-wave :thunderbolt :thundershock :body-slam :double-edge :mimic]
  :umbreon [:confuse-ray :faint-attack :mean-look :moonlight :pursuit :screech :snatch :taunt :torment :body-slam :double-edge :mimic :nightmare]
  :vaporeon [:acid-armor :aurora-beam :blizzard :dive :hail :haze :hydro-pump :ice-beam :icy-wind :surf :water-gun :water-pulse :waterfall :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :eevee [:attract :baton-pass :bite :captivate :charm :covet :curse :detect :dig :double-team :endure :facade :fake-tears :flail :frustration :growl :headbutt :heal-bell :helping-hand :hidden-power :iron-tail :last-resort :mud-slap :natural-gift :protect :quick-attack :rain-dance :rest :return :sand-attack :secret-power :shadow-ball :sleep-talk :snore :substitute :sunny-day :swagger :swift :tackle :tail-whip :take-down :tickle :toxic :trump-card :wish :yawn :body-slam :double-edge :mimic]
  :espeon [:calm-mind :confusion :cut :dream-eater :flash :future-sight :giga-impact :grass-knot :hyper-beam :light-screen :magic-coat :morning-sun :power-swap :psybeam :psych-up :psychic :reflect :signal-beam :skill-swap :trick :trick-room :zen-headbutt :body-slam :double-edge :mimic :nightmare]
  :flareon [:ember :fire-blast :fire-fang :fire-spin :flamethrower :heat-wave :lava-plume :overheat :roar :rock-smash :scary-face :smog :strength :superpower :will-o-wisp :body-slam :double-edge :mimic]
  :glaceon [:aqua-tail :avalanche :barrier :blizzard :hail :ice-beam :ice-fang :ice-shard :icy-wind :mirror-coat :water-pulse]
  :jolteon [:agility :charge-beam :discharge :double-kick :magnet-rise :pin-missile :shock-wave :thunder :thunder-fang :thunder-wave :thunderbolt :thundershock :body-slam :double-edge :mimic]
  :leafeon [:aerial-ace :bullet-seed :energy-ball :fury-cutter :giga-drain :grasswhistle :knock-off :leaf-blade :magical-leaf :razor-leaf :seed-bomb :solarbeam :swords-dance :synthesis :worry-seed :x-scissor]
  :umbreon [:assurance :confuse-ray :dark-pulse :faint-attack :guard-swap :mean-look :moonlight :payback :pursuit :screech :snatch :spite :sucker-punch :taunt :torment :body-slam :double-edge :mimic :nightmare]
  :vaporeon [:acid-armor :aqua-ring :aurora-beam :brine :dive :haze :hydro-pump :muddy-water :surf :water-gun :waterfall :whirlpool :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :eevee [:attract :baton-pass :bite :charm :covet :curse :detect :dig :double-edge :double-team :echoed-voice :endure :facade :fake-tears :flail :frustration :growl :heal-bell :helping-hand :hidden-power :hyper-voice :iron-tail :last-resort :natural-gift :protect :quick-attack :rain-dance :rest :retaliate :return :round :sand-attack :shadow-ball :sleep-talk :snore :stored-power :substitute :sunny-day :swagger :synchronoise :tackle :tail-whip :take-down :tickle :toxic :trump-card :wish :work-up :yawn :body-slam :double-edge :mimic]
  :espeon [:calm-mind :confusion :cut :dream-eater :flash :future-sight :giga-impact :grass-knot :hyper-beam :light-screen :magic-coat :magic-room :morning-sun :power-swap :psybeam :psych-up :psychic :psyshock :reflect :signal-beam :skill-swap :swift :telekinesis :trick :trick-room :zen-headbutt :body-slam :double-edge :mimic :nightmare]
  :flareon [:ember :fire-blast :fire-fang :fire-spin :flame-charge :flamethrower :heat-wave :incinerate :lava-plume :overheat :roar :rock-smash :scary-face :smog :strength :superpower :will-o-wisp :body-slam :double-edge :mimic]
  :glaceon [:aqua-tail :barrier :blizzard :frost-breath :hail :ice-beam :ice-fang :ice-shard :icy-wind :mirror-coat]
  :jolteon [:agility :charge-beam :discharge :double-kick :magnet-rise :pin-missile :thunder :thunder-fang :thunder-wave :thunderbolt :thundershock :volt-switch :wild-charge :body-slam :double-edge :mimic]
  :leafeon [:aerial-ace :energy-ball :giga-drain :grasswhistle :knock-off :leaf-blade :magical-leaf :razor-leaf :seed-bomb :solarbeam :swords-dance :synthesis :worry-seed :x-scissor]
  :umbreon [:assurance :confuse-ray :dark-pulse :faint-attack :foul-play :guard-swap :mean-look :moonlight :payback :pursuit :screech :snarl :snatch :spite :taunt :torment :wonder-room :body-slam :double-edge :mimic :nightmare]
  :vaporeon [:acid-armor :aqua-ring :aurora-beam :dive :haze :hydro-pump :muddy-water :scald :surf :water-gun :water-pulse :waterfall :body-slam :double-edge :icy-wind :mimic])
