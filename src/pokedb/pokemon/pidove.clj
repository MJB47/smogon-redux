
(in-ns 'smogon.dex)


(defpokemon :pidove
  :name "Pidove"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :rivalry :super-luck]]
  :stats [[50 55 50 36 30 43]]
  :weight [2.1]
  :height [0.3])

(defpokemon :tranquill
  :name "Tranquill"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :rivalry :super-luck]]
  :stats [[62 77 62 50 42 65]]
  :weight [15]
  :height [0.6])

(defpokemon :unfezant
  :name "Unfezant"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :rivalry :super-luck]]
  :stats [[80 105 80 65 55 93]]
  :weight [29]
  :height [1.2])

(deffamily (familychain :pidove :tranquill :unfezant))

(deflearnset :bw
  :pidove [:aerial-ace :air-cutter :air-slash :attract :bestow :detect :double-team :echoed-voice :facade :featherdance :fly :frustration :growl :gust :heat-wave :hidden-power :hypnosis :leer :lucky-chant :morning-sun :pluck :protect :quick-attack :rain-dance :razor-wind :rest :return :roost :round :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :tailwind :taunt :toxic :u-turn :uproar :wish :work-up]
  :unfezant [:giga-impact :hyper-beam :psych-up])
