
(in-ns 'smogon.dex)

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

(deffamily (familyclique :thundurus :thundurus-t))

(deflearnset :bw
  :thundurus [:agility :astonish :attract :bite :brick-break :bulk-up :charge :charge-beam :crunch :dark-pulse :discharge :double-team :embargo :facade :flash-cannon :fling :fly :focus-blast :foul-play :frustration :giga-impact :grass-knot :hammer-arm :heal-block :hidden-power :hyper-beam :incinerate :iron-tail :knock-off :nasty-plot :payback :protect :psychic :rain-dance :rest :return :revenge :rock-smash :role-play :round :shock-wave :sky-drop :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :strength :substitute :superpower :swagger :taunt :thief :thrash :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :torment :toxic :u-turn :uproar :volt-switch :wild-charge])
