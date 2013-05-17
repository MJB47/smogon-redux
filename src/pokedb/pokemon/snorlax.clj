
(in-ns 'smogon.dex)


(defpokemon :munchlax
  :name "Munchlax"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:gluttony :pickup :thick-fat]]
  :stats [[135 85 40 40 85 5]]
  :weight [105]
  :height [0.6])

(defpokemon :snorlax
  :name "Snorlax"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:monster]]
  :abilities [[:immunity :thick-fat]
              :bw [:gluttony :immunity :thick-fat]]
  :stats [[160 110 65 65 30]
          :gs [160 110 65 65 110 30]]
  :weight [460]
  :height [2.1])

(deffamily (familychain :munchlax :snorlax))

(deflearnset :rb
  :snorlax [:amnesia :bide :blizzard :body-slam :bubblebeam :counter :double-edge :double-team :earthquake :fire-blast :fissure :harden :headbutt :hyper-beam :ice-beam :mega-kick :mega-punch :metronome :mimic :pay-day :psychic :psywave :rage :reflect :rest :rock-slide :seismic-toss :selfdestruct :skull-bash :solarbeam :strength :submission :substitute :surf :take-down :thunder :thunderbolt :toxic :water-gun])

(deflearnset :gs
  :snorlax [:amnesia :attract :belly-drum :blizzard :body-slam :charm :curse :defense-curl :double-team :dynamicpunch :earthquake :endure :fire-blast :fire-punch :flamethrower :frustration :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :lick :mud-slap :protect :psych-up :psychic :rain-dance :rest :return :rock-smash :rollout :sandstorm :shadow-ball :sleep-talk :snore :solarbeam :strength :sunny-day :surf :swagger :tackle :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :snorlax [:amnesia :attract :belly-drum :blizzard :block :body-slam :brick-break :charm :counter :covet :curse :defense-curl :double-edge :double-team :dynamicpunch :earthquake :endure :facade :fire-blast :fire-punch :fissure :flamethrower :focus-punch :frustration :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :lick :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :psych-up :psychic :rain-dance :rest :return :rock-slide :rock-tomb :rollout :sandstorm :secret-power :seismic-toss :selfdestruct :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :surf :swagger :tackle :thunder :thunderbolt :thunderpunch :toxic :water-pulse :yawn :icy-wind :icy-wind :mimic :refresh :seismic-toss])

(deflearnset :dp
  :munchlax [:amnesia :attract :blizzard :body-slam :brick-break :captivate :charm :counter :curse :defense-curl :double-edge :double-team :earthquake :endure :facade :fire-blast :fire-punch :flamethrower :fling :focus-punch :frustration :gunk-shot :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :last-resort :lick :metronome :mud-slap :natural-gift :odor-sleuth :protect :psychic :pursuit :rain-dance :recycle :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :screech :secret-power :seed-bomb :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :stockpile :strength :substitute :sunny-day :superpower :surf :swagger :swallow :tackle :thunder :thunderbolt :thunderpunch :toxic :uproar :water-pulse :whirlpool :whirlwind :zen-headbutt]
  :snorlax [:belly-drum :block :crunch :fissure :focus-blast :giga-impact :hyper-beam :iron-head :outrage :yawn :icy-wind :icy-wind :mimic :refresh :seismic-toss])

(deflearnset :bw
  :munchlax [:after-you :amnesia :attract :blizzard :body-slam :brick-break :bulldoze :charm :chip-away :counter :covet :curse :defense-curl :double-edge :double-team :earthquake :facade :fire-blast :fire-punch :flamethrower :fling :frustration :gunk-shot :hidden-power :hyper-voice :ice-beam :ice-punch :icy-wind :incinerate :last-resort :lick :metronome :natural-gift :odor-sleuth :protect :psychic :pursuit :rain-dance :recycle :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :screech :seed-bomb :selfdestruct :shadow-ball :sleep-talk :snatch :snore :solarbeam :stockpile :strength :substitute :sunny-day :superpower :surf :swagger :swallow :tackle :thunder :thunderbolt :thunderpunch :toxic :uproar :whirlwind :work-up :zen-headbutt]
  :snorlax [:belly-drum :block :crunch :fissure :focus-blast :giga-impact :heavy-slam :hyper-beam :iron-head :outrage :smack-down :wild-charge :yawn :icy-wind :mimic :refresh :seismic-toss])
