
(in-ns 'smogon.dex)


(defpokemon :joltik
  :name "Joltik"
  :introduced-in :bw
  :types [[:bug :electric]]
  :egggroups [[:bug]]
  :abilities [[:compoundeyes :swarm :unnerve]]
  :stats [[50 47 50 57 50 65]]
  :weight [0.6]
  :height [0.1])

(defpokemon :galvantula
  :name "Galvantula"
  :introduced-in :bw
  :types [[:bug :electric]]
  :egggroups [[:bug]]
  :abilities [[:compoundeyes :swarm :unnerve]]
  :stats [[70 77 60 97 60 108]]
  :weight [14.3]
  :height [0.8])

(deffamily (familychain :joltik :galvantula))

(deflearnset :bw
  :joltik [:agility :attract :bounce :bug-bite :bug-buzz :charge-beam :cross-poison :cut :disable :discharge :double-team :electro-ball :electroweb :energy-ball :facade :faint-attack :flash :frustration :fury-cutter :gastro-acid :giga-drain :hidden-power :leech-life :light-screen :magnet-rise :pin-missile :poison-jab :poison-sting :protect :pursuit :rain-dance :rest :return :rock-climb :round :screech :signal-beam :slash :sleep-talk :snore :spider-web :string-shot :struggle-bug :substitute :sucker-punch :swagger :thief :thunder-wave :thunderbolt :toxic :volt-switch :wild-charge :x-scissor]
  :galvantula [:giga-impact :hyper-beam :thunder])
