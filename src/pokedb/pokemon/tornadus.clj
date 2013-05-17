
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

(deffamily (familyclique :tornadus :tornadus-t))

(deflearnset :bw
  :tornadus [:acrobatics :aerial-ace :agility :air-cutter :air-slash :astonish :attract :bite :brick-break :bulk-up :crunch :dark-pulse :double-team :embargo :extrasensory :facade :fling :fly :focus-blast :foul-play :frustration :giga-impact :grass-knot :gust :hammer-arm :heat-wave :hidden-power :hurricane :hyper-beam :icy-wind :incinerate :iron-tail :knock-off :payback :protect :psychic :rain-dance :rest :return :revenge :rock-smash :role-play :round :sky-drop :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :strength :substitute :superpower :swagger :tailwind :taunt :thief :thrash :torment :toxic :u-turn :uproar])
