
(in-ns 'smogon.dex)


(defpokemon :yamask
  :name "Yamask"
  :introduced-in :bw
  :types [[:ghost]]
  :egggroups [[:indeterminate :mineral]]
  :abilities [[:mummy]]
  :stats [[38 30 85 55 65 30]]
  :weight [1.5]
  :height [0.5])

(defpokemon :cofagrigus
  :name "Cofagrigus"
  :introduced-in :bw
  :types [[:ghost]]
  :egggroups [[:indeterminate :mineral]]
  :abilities [[:mummy]]
  :stats [[58 50 145 95 105 30]]
  :weight [76.5]
  :height [1.7])

(deffamily (familychain :yamask :cofagrigus))



(deflearnset :bw
  :yamask [:after-you :astonish :attract :block :calm-mind :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :endure :energy-ball :facade :fake-tears :flash :frustration :grudge :guard-split :haze :heal-block :hex :hidden-power :imprison :iron-defense :knock-off :magic-coat :mean-look :memento :nasty-plot :night-shade :nightmare :ominous-wind :pain-split :payback :power-split :protect :psych-up :psychic :rain-dance :rest :return :role-play :round :safeguard :shadow-ball :skill-swap :sleep-talk :snatch :snore :spite :substitute :swagger :telekinesis :thief :toxic :trick :trick-room :will-o-wisp :wonder-room]
  :cofagrigus [:giga-impact :grass-knot :hyper-beam :scary-face])

