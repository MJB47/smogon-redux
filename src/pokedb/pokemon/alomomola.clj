
(in-ns 'smogon.dex)


(defpokemon :alomomola
  :name "Alomomola"
  :introduced-in :bw
  :types [[:water]]
  :egggroups [[:water-1 :water-2]]
  :abilities [[:healer :hydration :regenerator]]
  :stats [[165 75 80 40 45 65]]
  :weight [31.6]
  :height [1.2])

(deffamily (familychain :alomomola))

(deflearnset :bw
  :alomomola [:aqua-jet :aqua-ring :attract :blizzard :bounce :brine :calm-mind :dive :double-team :doubleslap :endure :facade :frustration :hail :heal-pulse :healing-wish :helping-hand :hidden-power :hydro-pump :ice-beam :icy-wind :knock-off :light-screen :magic-coat :mirror-coat :mist :pain-split :pound :protect :psych-up :psychic :rain-dance :refresh :rest :return :round :safeguard :scald :shadow-ball :sleep-talk :snore :soak :substitute :surf :swagger :tickle :toxic :wake-up-slap :water-pulse :water-sport :waterfall :wide-guard :wish])
