
(in-ns 'smogon.dex)


(defpokemon :elgyem
  :name "Elgyem"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:analytic :synchronize :telepathy]]
  :stats [[55 55 55 85 55 30]]
  :weight [9]
  :height [0.5])

(defpokemon :beheeyem
  :name "Beheeyem"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:analytic :synchronize :telepathy]]
  :stats [[75 75 75 125 95 40]]
  :weight [34.5]
  :height [1])

(deffamily (familychain :elgyem :beheeyem))

(deflearnset :bw
  :elgyem [:after-you :ally-switch :astonish :attract :barrier :calm-mind :charge-beam :confusion :dark-pulse :disable :double-team :dream-eater :echoed-voice :embargo :energy-ball :facade :flash :frustration :gravity :growl :guard-split :guard-swap :headbutt :heal-block :hidden-power :imprison :light-screen :magic-coat :miracle-eye :nasty-plot :pain-split :power-split :power-swap :protect :psybeam :psych-up :psychic :psyshock :rain-dance :recover :recycle :reflect :rest :return :rock-slide :rock-tomb :role-play :round :safeguard :shadow-ball :signal-beam :simple-beam :skill-swap :sleep-talk :snatch :snore :substitute :swagger :synchronoise :telekinesis :teleport :thief :thunder-wave :thunderbolt :toxic :trick :trick-room :uproar :wonder-room :zen-headbutt]
  :beheeyem [:giga-impact :hyper-beam])
