
(in-ns 'smogon.dex)


(defpokemon :gothita
  :name "Gothita"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:frisk :shadow-tag]]
  :stats [[45 30 50 55 65 45]]
  :weight [5.8]
  :height [0.4])

(defpokemon :gothitelle
  :name "Gothitelle"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:frisk :shadow-tag]]
  :stats [[70 55 95 95 110 65]]
  :weight [44]
  :height [1.5])

(defpokemon :gothorita
  :name "Gothorita"
  :introduced-in :bw
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:frisk :shadow-tag]]
  :stats [[60 45 70 75 85 55]]
  :weight [18]
  :height [0.7])

(deffamily (familychain :gothita :gothorita :gothitelle))

(deflearnset :bw
  :gothita [:attract :calm-mind :captivate :charge-beam :charm :confusion :covet :dark-pulse :double-team :doubleslap :dream-eater :embargo :energy-ball :facade :faint-attack :fake-tears :flash :flatter :fling :foul-play :frustration :future-sight :grass-knot :gravity :heal-bell :heal-block :helping-hand :hidden-power :light-screen :magic-coat :magic-room :mean-look :miracle-eye :mirror-coat :payback :pound :protect :psybeam :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :rock-slide :rock-tomb :role-play :round :safeguard :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :swagger :taunt :telekinesis :thief :thunder-wave :thunderbolt :tickle :torment :toxic :trick :trick-room :uproar :zen-headbutt]
  :gothitelle [:brick-break :giga-impact :hyper-beam :low-sweep])
