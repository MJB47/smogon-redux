
(in-ns 'smogon.dex)


(defpokemon :panpour
  :name "Panpour"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:gluttony :torrent]]
  :stats [[50 53 48 53 48 64]]
  :weight [13.5]
  :height [0.6])

(defpokemon :simipour
  :name "Simipour"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:gluttony :torrent]]
  :stats [[75 98 63 98 63 101]]
  :weight [29]
  :height [1])

(deffamily (familychain :panpour :simipour))

(deflearnset :bw
  :panpour [:acrobatics :aqua-ring :aqua-tail :astonish :attract :bite :blizzard :brine :covet :crunch :cut :dig :dive :double-team :endeavor :facade :fling :frustration :fury-swipes :grass-knot :gunk-shot :hail :helping-hand :hidden-power :hone-claws :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :knock-off :leer :lick :low-kick :low-sweep :mud-sport :nasty-plot :natural-gift :payback :protect :rain-dance :recycle :rest :return :rock-smash :rock-tomb :role-play :round :scald :scratch :shadow-claw :sleep-talk :snore :substitute :surf :swagger :taunt :thief :tickle :torment :toxic :uproar :water-gun :water-sport :waterfall :work-up]
  :simipour [:brick-break :focus-blast :giga-impact :hyper-beam :rock-slide :superpower])
