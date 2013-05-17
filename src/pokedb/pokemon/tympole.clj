
(in-ns 'smogon.dex)


(defpokemon :tympole
  :name "Tympole"
  :introduced-in :bw
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:hydration :swift-swim :water-absorb]]
  :stats [[50 50 40 50 40 64]]
  :weight [4.5]
  :height [0.5])

(defpokemon :palpitoad
  :name "Palpitoad"
  :introduced-in :bw
  :types [[:ground :water]]
  :egggroups [[:water-1]]
  :abilities [[:hydration :swift-swim :water-absorb]]
  :stats [[75 65 55 65 55 69]]
  :weight [17]
  :height [0.8])

(defpokemon :seismitoad
  :name "Seismitoad"
  :introduced-in :bw
  :types [[:ground :water]]
  :egggroups [[:water-1]]
  :abilities [[:poison-touch :swift-swim :water-absorb]]
  :stats [[105 85 75 85 75 74]]
  :weight [62]
  :height [1.5])

(deffamily (familychain :tympole :palpitoad :seismitoad))



(deflearnset :bw
  :tympole [:aqua-ring :attract :bounce :bubble :bubblebeam :double-team :earth-power :echoed-voice :endeavor :facade :flail :frustration :growl :hail :hidden-power :hydro-pump :hyper-voice :icy-wind :mist :mud-bomb :mud-shot :mud-sport :muddy-water :protect :rain-dance :refresh :rest :return :round :scald :sleep-talk :sludge-bomb :sludge-wave :snore :substitute :supersonic :surf :swagger :toxic :uproar :water-pulse]
  :palpitoad [:bulldoze :gastro-acid :rock-smash :stealth-rock]
  :seismitoad [:acid :brick-break :dig :drain-punch :earthquake :fling :focus-blast :giga-impact :grass-knot :hyper-beam :ice-punch :knock-off :low-kick :payback :poison-jab :rock-slide :rock-tomb :strength :venoshock])

