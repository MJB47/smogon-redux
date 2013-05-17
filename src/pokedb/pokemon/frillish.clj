
(in-ns 'smogon.dex)


(defpokemon :frillish
  :name "Frillish"
  :introduced-in :bw
  :types [[:ghost :water]]
  :egggroups [[:indeterminate]]
  :abilities [[:cursed-body :damp :water-absorb]]
  :stats [[55 40 50 65 85 40]]
  :weight [33]
  :height [1.2])

(defpokemon :jellicent
  :name "Jellicent"
  :introduced-in :bw
  :types [[:ghost :water]]
  :egggroups [[:indeterminate]]
  :abilities [[:cursed-body :damp :water-absorb]]
  :stats [[100 60 70 85 105 60]]
  :weight [135]
  :height [2.2])

(deffamily (familychain :frillish :jellicent))

(deflearnset :bw
  :frillish [:absorb :acid-armor :attract :bind :blizzard :brine :bubble :bubblebeam :confuse-ray :constrict :dark-pulse :dive :double-team :dream-eater :energy-ball :facade :flash :frustration :giga-drain :hail :hex :hidden-power :hydro-pump :ice-beam :icy-wind :magic-coat :mist :night-shade :ominous-wind :pain-split :protect :psych-up :psychic :rain-dance :recover :rest :return :round :safeguard :scald :shadow-ball :sleep-talk :sludge-bomb :sludge-wave :snore :spite :substitute :surf :swagger :taunt :toxic :trick :trick-room :water-pulse :water-sport :water-spout :waterfall :will-o-wisp :wring-out]
  :jellicent [:giga-impact :hyper-beam])
