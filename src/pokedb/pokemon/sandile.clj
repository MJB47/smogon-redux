
(in-ns 'smogon.dex)


(defpokemon :sandile
  :name "Sandile"
  :introduced-in :bw
  :types [[:dark :ground]]
  :egggroups [[:ground]]
  :abilities [[:anger-point :intimidate :moxie]]
  :stats [[50 72 35 35 35 65]]
  :weight [15.2]
  :height [0.7])

(defpokemon :krokorok
  :name "Krokorok"
  :introduced-in :bw
  :types [[:dark :ground]]
  :egggroups [[:ground]]
  :abilities [[:anger-point :intimidate :moxie]]
  :stats [[60 82 45 45 45 74]]
  :weight [33.4]
  :height [1])

(defpokemon :krookodile
  :name "Krookodile"
  :introduced-in :bw
  :types [[:dark :ground]]
  :egggroups [[:ground]]
  :abilities [[:anger-point :intimidate :moxie]]
  :stats [[95 117 70 65 70 92]]
  :weight [96.3]
  :height [1.5])

(deffamily (familychain :sandile :krokorok :krookodile))

(deflearnset :bw
  :sandile [:aqua-tail :assurance :attract :beat-up :bite :bulldoze :counter :crunch :cut :dark-pulse :dig :double-edge :double-team :earth-power :earthquake :embargo :facade :fire-fang :focus-energy :foul-play :frustration :hidden-power :hone-claws :incinerate :iron-tail :leer :mean-look :mud-slap :payback :protect :pursuit :rage :rest :retaliate :return :roar :rock-climb :rock-slide :rock-tomb :round :sand-attack :sand-tomb :sandstorm :scary-face :sleep-talk :sludge-bomb :snarl :snatch :snore :spite :stealth-rock :stone-edge :substitute :swagger :taunt :thief :thrash :thunder-fang :torment :toxic :uproar]
  :krokorok [:brick-break :fling :grass-knot :knock-off :low-kick :low-sweep :rock-smash :shadow-claw :strength]
  :krookodile [:aerial-ace :block :bulk-up :dragon-claw :dragon-pulse :dragon-tail :focus-blast :giga-impact :hyper-beam :outrage :smack-down :superpower])
