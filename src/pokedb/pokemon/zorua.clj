
(in-ns 'smogon.dex)


(defpokemon :zorua
  :name "Zorua"
  :introduced-in :bw
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:illusion]]
  :stats [[40 65 40 80 40 65]]
  :weight [12.5]
  :height [0.7])

(defpokemon :zoroark
  :name "Zoroark"
  :introduced-in :bw
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:illusion]]
  :stats [[60 105 60 120 60 105]]
  :weight [81.1]
  :height [1.6])

(deffamily (familychain :zorua :zoroark))



(deflearnset :bw
  :zorua [:aerial-ace :agility :attract :bounce :calm-mind :captivate :counter :covet :cut :dark-pulse :detect :dig :double-team :embargo :extrasensory :facade :faint-attack :fake-tears :fling :foul-play :frustration :fury-swipes :grass-knot :hidden-power :hone-claws :hyper-voice :imprison :incinerate :knock-off :leer :memento :nasty-plot :night-daze :payback :protect :psych-up :punishment :pursuit :rain-dance :rest :retaliate :return :roar :round :scary-face :scratch :shadow-ball :sleep-talk :snarl :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :swords-dance :taunt :thief :torment :toxic :trick :u-turn :uproar]
  :zoroark [:flamethrower :focus-blast :giga-impact :hyper-beam :low-kick :low-sweep :night-slash :rock-smash :shadow-claw])

