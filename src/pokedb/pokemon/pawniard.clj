
(in-ns 'smogon.dex)


(defpokemon :pawniard
  :name "Pawniard"
  :introduced-in :bw
  :types [[:dark :steel]]
  :egggroups [[:humanshape]]
  :abilities [[:defiant :inner-focus :pressure]]
  :stats [[45 85 70 40 40 60]]
  :weight [10.2]
  :height [0.5])

(defpokemon :bisharp
  :name "Bisharp"
  :introduced-in :bw
  :types [[:dark :steel]]
  :egggroups [[:humanshape]]
  :abilities [[:defiant :inner-focus :pressure]]
  :stats [[65 125 100 60 70 70]]
  :weight [70]
  :height [1.6])

(deffamily (familychain :pawniard :bisharp))

(deflearnset :bw
  :pawniard [:aerial-ace :assurance :attract :brick-break :cut :dark-pulse :dig :double-team :dual-chop :embargo :facade :faint-attack :false-swipe :fling :foul-play :frustration :fury-cutter :grass-knot :guillotine :headbutt :hidden-power :hone-claws :iron-defense :iron-head :knock-off :leer :low-kick :low-sweep :magnet-rise :mean-look :metal-claw :metal-sound :night-slash :payback :poison-jab :protect :psycho-cut :pursuit :rain-dance :rest :retaliate :return :revenge :rock-polish :rock-smash :rock-tomb :role-play :round :sandstorm :scary-face :scratch :shadow-claw :slash :sleep-talk :snarl :snatch :snore :spite :stealth-rock :substitute :sucker-punch :swagger :swords-dance :taunt :thief :thunder-wave :torment :toxic :x-scissor]
  :bisharp [:focus-blast :giga-impact :hyper-beam :metal-burst :stone-edge])
