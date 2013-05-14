
(in-ns 'smogon.dex)


(defpokemon :tornadus
  :name "Tornadus"
  :introduced-in :bw
  :types [[:flying]]
  :egggroups [[]]
  :abilities [[:defiant :prankster]]
  :stats [[79 115 70 125 80 111]]
  :weight [63]
  :height [1.5])

(defpokemon :tornadus-t
  :name "Tornadus-Therian"
  :introduced-in :bw
  :types [[:flying]]
  :egggroups [[]]
  :abilities [[:regenerator]]
  :stats [[79 100 80 110 90 121]]
  :weight [63]
  :height [1.4])

(defpokemon :thundurus
  :name "Thundurus"
  :introduced-in :bw
  :types [[:electric :flying]]
  :egggroups [[]]
  :abilities [[:defiant :prankster]]
  :stats [[79 115 70 125 80 111]]
  :weight [61]
  :height [1.5])

(defpokemon :thundurus-t
  :name "Thundurus-Therian"
  :introduced-in :bw
  :types [[:electric :flying]]
  :egggroups [[]]
  :abilities [[:volt-absorb]]
  :stats [[79 105 70 145 80 101]]
  :weight [61]
  :height [3])

(defpokemon :landorus
  :name "Landorus"
  :introduced-in :bw
  :types [[:flying :ground]]
  :egggroups [[]]
  :abilities [[:sand-force :sheer-force]]
  :stats [[89 125 90 115 80 101]]
  :weight [68]
  :height [1.5])

(defpokemon :landorus-t
  :name "Landorus-Therian"
  :introduced-in :bw
  :types [[:flying :ground]]
  :egggroups [[]]
  :abilities [[:intimidate]]
  :stats [[89 145 90 105 80 91]]
  :weight [68]
  :height [1.3])

(deffamilygraph (familyclique :thundurus :thundurus-t))
(deffamilygraph (familyclique :tornadus :tornadus-t))
(deffamilygraph (familyclique :landorus :landorus-t))

(deflearnset :bw
  :landorus [:attract :block :brick-break :bulk-up :bulldoze :calm-mind :dig :double-team :earth-power :earthquake :explosion :extrasensory :facade :fissure :fling :fly :focus-blast :frustration :giga-impact :grass-knot :gravity :hammer-arm :hidden-power :hyper-beam :imprison :iron-tail :knock-off :mud-shot :outrage :payback :protect :psychic :punishment :rest :return :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :role-play :round :sandstorm :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :stealth-rock :stone-edge :strength :substitute :superpower :swagger :swords-dance :toxic :u-turn])




(deflearnset :bw
  :thundurus [:agility :astonish :attract :bite :brick-break :bulk-up :charge :charge-beam :crunch :dark-pulse :discharge :double-team :embargo :facade :flash-cannon :fling :fly :focus-blast :foul-play :frustration :giga-impact :grass-knot :hammer-arm :heal-block :hidden-power :hyper-beam :incinerate :iron-tail :knock-off :nasty-plot :payback :protect :psychic :rain-dance :rest :return :revenge :rock-smash :role-play :round :shock-wave :sky-drop :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :strength :substitute :superpower :swagger :taunt :thief :thrash :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :torment :toxic :u-turn :uproar :volt-switch :wild-charge])




(deflearnset :bw
  :tornadus [:acrobatics :aerial-ace :agility :air-cutter :air-slash :astonish :attract :bite :brick-break :bulk-up :crunch :dark-pulse :double-team :embargo :extrasensory :facade :fling :fly :focus-blast :foul-play :frustration :giga-impact :grass-knot :gust :hammer-arm :heat-wave :hidden-power :hurricane :hyper-beam :icy-wind :incinerate :iron-tail :knock-off :payback :protect :psychic :rain-dance :rest :return :revenge :rock-smash :role-play :round :sky-drop :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :strength :substitute :superpower :swagger :tailwind :taunt :thief :thrash :torment :toxic :u-turn :uproar])
