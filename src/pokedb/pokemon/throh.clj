
(in-ns 'smogon.dex)


(defpokemon :throh
  :name "Throh"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :inner-focus :mold-breaker]]
  :stats [[120 100 85 30 85 45]]
  :weight [55.5]
  :height [1.3])

(deffamily (familychain :throh))

(deflearnset :bw
  :throh [:attract :bide :bind :block :body-slam :brick-break :bulk-up :bulldoze :circle-throw :dig :double-team :earthquake :endure :facade :fire-punch :fling :focus-blast :focus-energy :frustration :giga-impact :grass-knot :helping-hand :hidden-power :ice-punch :knock-off :leer :low-kick :low-sweep :pain-split :payback :poison-jab :protect :rain-dance :rest :retaliate :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :round :seismic-toss :sleep-talk :snore :stone-edge :storm-throw :strength :substitute :sunny-day :superpower :swagger :taunt :thunderpunch :toxic :vital-throw :wide-guard :work-up])

