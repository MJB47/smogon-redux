
(in-ns 'smogon.dex)


(defpokemon :timburr
  :name "Timburr"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :iron-fist :sheer-force]]
  :stats [[75 80 55 25 35 35]]
  :weight [12.5]
  :height [0.6])

(defpokemon :gurdurr
  :name "Gurdurr"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :iron-fist :sheer-force]]
  :stats [[85 105 85 40 50 40]]
  :weight [40]
  :height [1.2])

(defpokemon :conkeldurr
  :name "Conkeldurr"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:humanshape]]
  :abilities [[:guts :iron-fist :sheer-force]]
  :stats [[105 140 95 55 65 45]]
  :weight [87]
  :height [1.4])

(deffamily (familychain :timburr :gurdurr :conkeldurr))



(deflearnset :bw
  :timburr [:attract :bide :block :brick-break :bulk-up :chip-away :comet-punch :counter :detect :dig :double-team :drain-punch :dynamicpunch :endure :facade :fire-punch :fling :focus-blast :focus-energy :focus-punch :force-palm :foresight :frustration :grass-knot :hammer-arm :helping-hand :hidden-power :ice-punch :knock-off :leer :low-kick :low-sweep :mach-punch :payback :poison-jab :pound :protect :rain-dance :rest :retaliate :return :reversal :rock-slide :rock-smash :rock-throw :rock-tomb :round :scary-face :sleep-talk :smack-down :smellingsalt :snore :stone-edge :strength :substitute :sunny-day :superpower :swagger :taunt :thunderpunch :toxic :wake-up-slap :wide-guard :work-up]
  :conkeldurr [:bulldoze :earthquake :giga-impact :hyper-beam])

