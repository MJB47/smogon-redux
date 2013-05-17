
(in-ns 'smogon.dex)


(defpokemon :purrloin
  :name "Purrloin"
  :introduced-in :dp
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:limber :prankster :unburden]]
  :stats [[41 50 37 50 37 66]]
  :weight [10.1]
  :height [0.4])

(defpokemon :liepard
  :name "Liepard"
  :introduced-in :dp
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:limber :prankster :unburden]]
  :stats [[64 88 50 88 50 106]]
  :weight [37.5]
  :height [1.1])

(deffamily (familychain :purrloin :liepard))

(deflearnset :bw
  :purrloin [:aerial-ace :assist :assurance :attract :captivate :charm :covet :cut :dark-pulse :double-team :dream-eater :echoed-voice :embargo :encore :facade :faint-attack :fake-out :fake-tears :foul-play :frustration :fury-swipes :grass-knot :growl :gunk-shot :hidden-power :hone-claws :hyper-voice :iron-tail :knock-off :nasty-plot :night-slash :pay-day :payback :protect :psych-up :pursuit :rain-dance :rest :return :role-play :round :sand-attack :scratch :seed-bomb :shadow-ball :shadow-claw :slash :sleep-talk :snarl :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :thunder-wave :torment :toxic :trick :u-turn :yawn]
  :liepard [:giga-impact :hyper-beam :rock-smash])
