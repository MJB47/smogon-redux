
(in-ns 'smogon.dex)


(defpokemon :axew
  :name "Axew"
  :introduced-in :bw
  :types [[:dragon]]
  :egggroups [[:dragon :monster]]
  :abilities [[:mold-breaker :rivalry :unnerve]]
  :stats [[46 87 60 30 40 57]]
  :weight [18]
  :height [0.6])

(defpokemon :fraxure
  :name "Fraxure"
  :introduced-in :bw
  :types [[:dragon]]
  :egggroups [[:dragon :monster]]
  :abilities [[:mold-breaker :rivalry :unnerve]]
  :stats [[66 117 70 40 50 67]]
  :weight [36]
  :height [1])

(defpokemon :haxorus
  :name "Haxorus"
  :introduced-in :bw
  :types [[:dragon]]
  :egggroups [[:dragon :monster]]
  :abilities [[:mold-breaker :rivalry :unnerve]]
  :stats [[76 147 90 60 70 97]]
  :weight [105.5]
  :height [1.8])

(deffamily (familychain :axew :fraxure :haxorus))

(deflearnset :bw
  :axew [:aerial-ace :aqua-tail :assurance :attract :counter :cut :dig :double-team :draco-meteor :dragon-claw :dragon-dance :dragon-pulse :dragon-rage :dual-chop :endeavor :endure :facade :false-swipe :fling :focus-energy :frustration :giga-impact :guillotine :harden :hidden-power :hone-claws :incinerate :iron-tail :leer :night-slash :outrage :payback :poison-jab :protect :rain-dance :razor-wind :rest :return :reversal :roar :rock-smash :rock-tomb :round :scary-face :scratch :slash :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :swords-dance :taunt :toxic :x-scissor]
  :fraxure [:dragon-tail :low-kick :shadow-claw]
  :haxorus [:brick-break :bulldoze :earthquake :focus-blast :grass-knot :hyper-beam :rock-slide :surf])
