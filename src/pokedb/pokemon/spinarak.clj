
(in-ns 'smogon.dex)


(defpokemon :spinarak
  :name "Spinarak"
  :introduced-in :gs
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:insomnia :swarm]
              :bw [:insomnia :sniper :swarm]]
  :stats [[40 60 40 40 40 30]]
  :weight [8.5]
  :height [0.5])

(defpokemon :ariados
  :name "Ariados"
  :introduced-in :gs
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:insomnia :swarm]
              :bw [:insomnia :sniper :swarm]]
  :stats [[70 90 70 60 60 40]]
  :weight [33.5]
  :height [1.1])

(deffamily (familychain :spinarak :ariados))



(deflearnset :gs
  :spinarak [:agility :attract :baton-pass :constrict :curse :dig :disable :double-team :endure :flash :frustration :fury-swipes :giga-drain :hidden-power :leech-life :night-shade :poison-sting :protect :psybeam :psychic :pursuit :rest :return :scary-face :screech :sleep-talk :sludge-bomb :snore :solarbeam :sonicboom :spider-web :string-shot :sunny-day :swagger :thief :toxic]
  :ariados [:hyper-beam])

(deflearnset :rs
  :spinarak [:agility :attract :baton-pass :body-slam :constrict :dig :disable :double-edge :double-team :endure :facade :flash :frustration :fury-swipes :giga-drain :hidden-power :leech-life :mimic :night-shade :poison-sting :protect :psybeam :psychic :pursuit :rest :return :scary-face :secret-power :signal-beam :sleep-talk :sludge-bomb :snore :solarbeam :sonicboom :spider-web :string-shot :substitute :sunny-day :swagger :thief :toxic :refresh]
  :ariados [:hyper-beam])

(deflearnset :dp
  :spinarak [:agility :attract :baton-pass :bounce :bug-bite :captivate :constrict :dig :disable :double-team :endure :facade :flash :frustration :fury-swipes :giga-drain :hidden-power :leech-life :natural-gift :night-shade :pin-missile :poison-jab :poison-sting :protect :psybeam :psychic :pursuit :rest :return :scary-face :secret-power :shadow-sneak :signal-beam :sleep-talk :sludge-bomb :solarbeam :sonicboom :spider-web :string-shot :substitute :sucker-punch :sunny-day :swagger :thief :toxic :toxic-spikes :refresh]
  :ariados [:giga-impact :hyper-beam])

(deflearnset :bw
  :spinarak [:agility :attract :baton-pass :bounce :bug-bite :constrict :cross-poison :dig :disable :double-team :electroweb :facade :flash :foul-play :frustration :fury-swipes :giga-drain :hidden-power :hone-claws :leech-life :night-shade :night-slash :pin-missile :poison-jab :poison-sting :protect :psybeam :psychic :pursuit :rage-powder :rest :return :round :scary-face :shadow-sneak :signal-beam :sleep-talk :sludge-bomb :solarbeam :sonicboom :spider-web :string-shot :struggle-bug :substitute :sucker-punch :sunny-day :swagger :thief :toxic :toxic-spikes :twineedle :venoshock :x-scissor :refresh]
  :ariados [:giga-impact :hyper-beam])

