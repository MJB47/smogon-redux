
(in-ns 'smogon.dex)


(defpokemon :litwick
  :name "Litwick"
  :introduced-in :bw
  :types [[:fire :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:flame-body :flash-fire :shadow-tag]]
  :stats [[50 30 55 65 55 20]]
  :weight [3.1]
  :height [0.3])

(defpokemon :lampent
  :name "Lampent"
  :introduced-in :bw
  :types [[:fire :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:flame-body :flash-fire :shadow-tag]]
  :stats [[60 40 60 95 60 55]]
  :weight [13]
  :height [0.6])

(defpokemon :chandelure
  :name "Chandelure"
  :introduced-in :bw
  :types [[:fire :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:flame-body :flash-fire :shadow-tag]]
  :stats [[60 55 90 145 90 80]]
  :weight [34.3]
  :height [1])

(deffamily (familychain :litwick :lampent :chandelure))

(deflearnset :bw
  :litwick [:acid :acid-armor :astonish :attract :calm-mind :captivate :clear-smog :confuse-ray :curse :dark-pulse :double-team :dream-eater :embargo :ember :endure :energy-ball :facade :fire-blast :fire-spin :flame-burst :flame-charge :flamethrower :flash :frustration :haze :heat-wave :hex :hidden-power :imprison :incinerate :inferno :memento :minimize :night-shade :overheat :pain-split :payback :protect :psych-up :psychic :rest :return :round :safeguard :shadow-ball :sleep-talk :smog :snore :solarbeam :spite :substitute :sunny-day :swagger :taunt :telekinesis :thief :toxic :trick :trick-room :will-o-wisp]
  :chandelure [:giga-impact :hyper-beam])
