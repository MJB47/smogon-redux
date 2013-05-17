
(in-ns 'smogon.dex)


(defpokemon :munna
  :name "Munna"
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[:ground]]
  :abilities [[:forewarn :synchronize :telepathy]]
  :stats [[76 25 45 67 55 24]]
  :weight [23.3]
  :height [0.6])

(defpokemon :musharna
  :name "Musharna"
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[:ground]]
  :abilities [[:forewarn :synchronize :telepathy]]
  :stats [[116 55 85 107 95 29]]
  :weight [60.5]
  :height [1.1])

(deffamily (familychain :munna :musharna))

(deflearnset :bw
  :munna [:after-you :attract :barrier :baton-pass :calm-mind :charge-beam :curse :defense-curl :double-team :dream-eater :energy-ball :facade :flash :frustration :future-sight :gravity :gyro-ball :heal-bell :helping-hand :hidden-power :hypnosis :imprison :light-screen :lucky-chant :magic-coat :moonlight :nightmare :pain-split :protect :psybeam :psych-up :psychic :psyshock :psywave :rain-dance :reflect :rest :return :rock-slide :rock-tomb :round :safeguard :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :sonicboom :stored-power :substitute :swagger :swift :synchronoise :telekinesis :thunder-wave :torment :toxic :trick :trick-room :wonder-room :worry-seed :yawn :zen-headbutt]
  :musharna [:giga-impact :hyper-beam])
