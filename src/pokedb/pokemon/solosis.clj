
(in-ns 'smogon.dex)


(defpokemon :solosis
  :name "Solosis"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:magic-guard :overcoat :regenerator]]
  :stats [[45 30 40 105 50 20]]
  :weight [1]
  :height [0.3])

(defpokemon :duosion
  :name "Duosion"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:magic-guard :overcoat :regenerator]]
  :stats [[65 40 50 125 60 30]]
  :weight [8]
  :height [0.6])

(defpokemon :reuniclus
  :name "Reuniclus"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:magic-guard :overcoat :regenerator]]
  :stats [[110 65 75 125 85 30]]
  :weight [20.1]
  :height [1])

(deffamily (familychain :solosis :duosion :reuniclus))



(deflearnset :bw
  :solosis [:acid-armor :after-you :astonish :attract :calm-mind :charm :confuse-ray :double-team :dream-eater :embargo :endeavor :energy-ball :explosion :facade :flash :flash-cannon :frustration :future-sight :gravity :gyro-ball :heal-block :hidden-power :imprison :light-screen :magic-coat :night-shade :pain-split :protect :psych-up :psychic :psyshock :psywave :rain-dance :recover :reflect :rest :return :rock-slide :rock-tomb :role-play :rollout :round :safeguard :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :swagger :telekinesis :thunder :thunder-wave :toxic :trick :trick-room :wonder-room :zen-headbutt]
  :reuniclus [:dizzy-punch :drain-punch :fire-punch :fling :focus-blast :giga-impact :grass-knot :helping-hand :hyper-beam :ice-punch :knock-off :rock-smash :strength :superpower :thunderpunch])

