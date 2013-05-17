
(in-ns 'smogon.dex)


(defpokemon :snubbull
  :name "Snubbull"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:fairy :ground]]
  :abilities [[:intimidate :run-away]
              :bw [:intimidate :rattled :run-away]]
  :stats [[60 80 50 40 40 30]]
  :weight [7.8]
  :height [0.6])

(defpokemon :granbull
  :name "Granbull"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:fairy :ground]]
  :abilities [[:intimidate]
              :dp [:intimidate :quick-feet]
              :bw [:intimidate :quick-feet :rattled]]
  :stats [[90 120 75 60 60 45]]
  :weight [48.7]
  :height [1.4])

(deffamily (familychain :snubbull :granbull))



(deflearnset :gs
  :snubbull [:attract :bite :charm :crunch :curse :defense-curl :detect :double-team :dynamicpunch :endure :faint-attack :fire-punch :frustration :headbutt :heal-bell :hidden-power :ice-punch :leer :lick :metronome :mud-slap :present :protect :rage :rain-dance :reflect :rest :return :roar :rock-smash :scary-face :shadow-ball :sleep-talk :sludge-bomb :snore :strength :sunny-day :swagger :tackle :tail-whip :take-down :thief :thunder :thunderbolt :thunderpunch :toxic :zap-cannon]
  :granbull [:hyper-beam])

(deflearnset :rs
  :snubbull [:attract :bite :body-slam :brick-break :bulk-up :charm :counter :crunch :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :faint-attack :fire-blast :fire-punch :flamethrower :focus-punch :frustration :heal-bell :hidden-power :ice-punch :lick :mega-kick :mega-punch :metronome :mimic :mud-slap :overheat :present :protect :rage :rain-dance :reflect :rest :return :roar :rock-smash :scary-face :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :sludge-bomb :smellingsalt :snore :solarbeam :strength :substitute :sunny-day :swagger :tackle :tail-whip :take-down :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :water-pulse :body-slam :seismic-toss]
  :granbull [:hyper-beam :iron-tail :rock-slide :rock-tomb :body-slam :seismic-toss])

(deflearnset :dp
  :snubbull [:attract :bite :brick-break :bulk-up :captivate :charm :close-combat :crunch :dig :double-team :earthquake :endure :facade :faint-attack :fire-blast :fire-fang :fire-punch :flamethrower :fling :focus-punch :frustration :headbutt :heal-bell :hidden-power :ice-fang :ice-punch :last-resort :lick :low-kick :metronome :mud-slap :natural-gift :overheat :payback :present :protect :rage :rain-dance :reflect :rest :return :roar :rock-smash :scary-face :secret-power :shadow-ball :shock-wave :sleep-talk :sludge-bomb :smellingsalt :snore :solarbeam :strength :substitute :sunny-day :super-fang :superpower :swagger :tackle :tail-whip :take-down :taunt :thief :thunder :thunder-fang :thunder-wave :thunderbolt :thunderpunch :torment :toxic :water-pulse :body-slam :seismic-toss]
  :granbull [:focus-blast :giga-impact :hyper-beam :iron-tail :rock-climb :rock-slide :rock-tomb :stone-edge :body-slam :seismic-toss])

(deflearnset :bw
  :snubbull [:attract :bite :brick-break :bulk-up :bulldoze :charm :close-combat :covet :crunch :dig :double-edge :double-team :earthquake :facade :faint-attack :fire-blast :fire-fang :fire-punch :flamethrower :fling :focus-punch :frustration :headbutt :heal-bell :hidden-power :hyper-voice :ice-fang :ice-punch :incinerate :last-resort :lick :low-kick :metronome :mimic :overheat :payback :present :protect :rage :rain-dance :reflect :rest :retaliate :return :roar :rock-smash :round :scary-face :shadow-ball :sleep-talk :sludge-bomb :smellingsalt :snarl :snore :solarbeam :strength :substitute :sunny-day :super-fang :superpower :swagger :tackle :tail-whip :take-down :taunt :thief :thunder :thunder-fang :thunder-wave :thunderbolt :thunderpunch :torment :toxic :wild-charge :work-up :body-slam :seismic-toss]
  :granbull [:focus-blast :giga-impact :hyper-beam :iron-tail :outrage :rock-slide :rock-tomb :stone-edge :body-slam :seismic-toss])

