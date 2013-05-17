
(in-ns 'smogon.dex)


(defpokemon :ducklett
  :name "Ducklett"
  :introduced-in :bw
  :types [[:flying :water]]
  :egggroups [[:flying :water-1]]
  :abilities [[:big-pecks :hydration :keen-eye]]
  :stats [[62 44 50 44 50 55]]
  :weight [5.5]
  :height [0.5])

(defpokemon :swanna
  :name "Swanna"
  :introduced-in :bw
  :types [[:flying :water]]
  :egggroups [[:flying :water-1]]
  :abilities [[:big-pecks :hydration :keen-eye]]
  :stats [[75 87 63 87 63 98]]
  :weight [24.2]
  :height [1.3])

(deffamily (familychain :ducklett :swanna))

(deflearnset :bw
  :ducklett [:aerial-ace :air-cutter :air-slash :aqua-ring :attract :brave-bird :brine :bubblebeam :defog :dive :double-team :endeavor :facade :featherdance :fly :frustration :gust :hail :hidden-power :hurricane :ice-beam :icy-wind :lucky-chant :me-first :mirror-move :pluck :protect :rain-dance :rest :return :roost :round :scald :sleep-talk :snore :steel-wing :substitute :surf :swagger :tailwind :toxic :uproar :water-gun :water-pulse :water-sport :wing-attack]
  :swanna [:giga-impact :hyper-beam :sky-attack])
