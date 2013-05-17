
(in-ns 'smogon.dex)


(defpokemon :scraggy
  :name "Scraggy"
  :introduced-in :bw
  :types [[:dark :fighting]]
  :egggroups [[:dragon :ground]]
  :abilities [[:intimidate :moxie :shed-skin]]
  :stats [[50 75 70 35 70 48]]
  :weight [11.8]
  :height [0.6])

(defpokemon :scrafty
  :name "Scrafty"
  :introduced-in :bw
  :types [[:dark :fighting]]
  :egggroups [[:dragon :ground]]
  :abilities [[:intimidate :moxie :shed-skin]]
  :stats [[65 90 115 45 115 58]]
  :weight [30]
  :height [1.1])

(deffamily (familychain :scraggy :scrafty))

(deflearnset :bw
  :scraggy [:amnesia :attract :brick-break :bulk-up :chip-away :counter :crunch :dark-pulse :detect :dig :double-team :dragon-claw :dragon-dance :dragon-pulse :dragon-tail :drain-punch :dual-chop :facade :faint-attack :fake-out :fire-punch :fling :focus-blast :focus-punch :foul-play :frustration :grass-knot :head-smash :headbutt :hi-jump-kick :hidden-power :ice-punch :incinerate :iron-defense :iron-head :iron-tail :knock-off :leer :low-kick :low-sweep :payback :poison-jab :protect :rain-dance :rest :retaliate :return :roar :rock-climb :rock-slide :rock-smash :rock-tomb :round :sand-attack :scary-face :sleep-talk :sludge-bomb :smack-down :snarl :snatch :snore :spite :stone-edge :strength :substitute :sunny-day :super-fang :swagger :taunt :thunderpunch :torment :toxic :work-up :zen-headbutt]
  :scrafty [:giga-impact :hyper-beam :outrage :thief])
