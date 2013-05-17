
(in-ns 'smogon.dex)

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

(deffamily (familyclique :landorus :landorus-t))

(deflearnset :bw
  :landorus [:attract :block :brick-break :bulk-up :bulldoze :calm-mind :dig :double-team :earth-power :earthquake :explosion :extrasensory :facade :fissure :fling :fly :focus-blast :frustration :giga-impact :grass-knot :gravity :hammer-arm :hidden-power :hyper-beam :imprison :iron-tail :knock-off :mud-shot :outrage :payback :protect :psychic :punishment :rest :return :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :role-play :round :sandstorm :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :stealth-rock :stone-edge :strength :substitute :superpower :swagger :swords-dance :toxic :u-turn])

