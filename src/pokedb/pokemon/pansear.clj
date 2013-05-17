
(in-ns 'smogon.dex)


(defpokemon :pansear
  :name "Pansear"
  :introduced-in :dp
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze :gluttony]]
  :stats [[50 53 48 53 48 64]]
  :weight [11]
  :height [0.6])

(defpokemon :simisear
  :name "Simisear"
  :introduced-in :dp
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze :gluttony]]
  :stats [[75 98 63 98 63 101]]
  :weight [28]
  :height [1])

(deffamily (familychain :pansear :simisear))

(deflearnset :bw
  :pansear [:acrobatics :amnesia :astonish :attract :bite :covet :crunch :cut :dig :double-team :endeavor :facade :fire-blast :fire-punch :fire-spin :flame-burst :flame-charge :flamethrower :fling :frustration :fury-swipes :grass-knot :gunk-shot :heat-wave :helping-hand :hidden-power :hone-claws :incinerate :iron-tail :knock-off :leer :lick :low-kick :low-sweep :nasty-plot :natural-gift :overheat :payback :protect :recycle :rest :return :rock-smash :rock-tomb :role-play :round :scratch :shadow-claw :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :taunt :thief :tickle :torment :toxic :uproar :will-o-wisp :work-up :yawn]
  :simisear [:brick-break :focus-blast :giga-impact :hyper-beam :rock-slide :superpower])
