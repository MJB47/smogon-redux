
(in-ns 'smogon.dex)


(defpokemon :pansage
  :name "Pansage"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground]]
  :abilities [[:gluttony :overgrow]]
  :stats [[50 53 48 53 48 64]]
  :weight [10.5]
  :height [0.6])

(defpokemon :simisage
  :name "Simisage"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground]]
  :abilities [[:gluttony :overgrow]]
  :stats [[75 98 63 98 63 101]]
  :weight [30.5]
  :height [1.1])

(deffamily (familychain :pansage :simisage))

(deflearnset :bw
  :pansage [:acrobatics :astonish :attract :bite :bullet-seed :covet :crunch :cut :dig :double-team :endeavor :energy-ball :facade :flash :fling :frustration :fury-swipes :giga-drain :grass-knot :grasswhistle :gunk-shot :helping-hand :hidden-power :hone-claws :iron-tail :knock-off :leaf-storm :leech-seed :leer :lick :low-kick :low-sweep :magical-leaf :nasty-plot :natural-gift :payback :protect :recycle :rest :return :rock-smash :rock-tomb :role-play :round :scratch :seed-bomb :shadow-claw :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :synthesis :taunt :thief :tickle :torment :toxic :uproar :vine-whip :work-up :worry-seed]
  :simisage [:brick-break :focus-blast :giga-impact :hyper-beam :rock-slide :superpower])
