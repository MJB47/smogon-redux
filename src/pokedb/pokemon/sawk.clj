
(in-ns 'smogon.dex)


(defpokemon :sawk
  :name "Sawk"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:inner-focus :mold-breaker :sturdy]]
  :stats [[75 125 75 30 75 85]]
  :weight [51]
  :height [1.4])

(deffamily (familychain :sawk))

(deflearnset :bw
  :sawk [:attract :bide :block :brick-break :bulk-up :bulldoze :close-combat :counter :dig :double-kick :double-team :dual-chop :earthquake :endure :facade :fire-punch :fling :focus-blast :focus-energy :frustration :giga-impact :grass-knot :helping-hand :hidden-power :ice-punch :karate-chop :knock-off :leer :low-kick :low-sweep :pain-split :payback :poison-jab :protect :quick-guard :rain-dance :rest :retaliate :return :reversal :rock-slide :rock-smash :rock-tomb :round :sleep-talk :snore :stone-edge :strength :substitute :sunny-day :superpower :swagger :taunt :thunderpunch :toxic :work-up])

