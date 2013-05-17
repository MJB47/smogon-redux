
(in-ns 'smogon.dex)


(defpokemon :surskit
  :name "Surskit"
  :introduced-in :rs
  :types [[:bug :water]]
  :egggroups [[:bug :water-1]]
  :abilities [[:swift-swim]
              :bw [:rain-dish :swift-swim]]
  :stats [[40 30 32 50 52 65]]
  :weight [1.7]
  :height [0.5])

(defpokemon :masquerain
  :name "Masquerain"
  :introduced-in :rs
  :types [[:bug :flying]]
  :egggroups [[:bug :water-1]]
  :abilities [[:intimidate]
              :bw [:intimidate :unnerve]]
  :stats [[70 60 62 80 82 60]]
  :weight [3.6]
  :height [0.8])

(deffamily (familychain :surskit :masquerain))



(deflearnset :rs
  :surskit [:agility :attract :blizzard :bubble :bubblebeam :double-edge :double-team :endure :facade :flash :foresight :frustration :giga-drain :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mimic :mind-reader :mist :mud-shot :protect :psybeam :psych-up :quick-attack :rain-dance :rest :return :secret-power :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swift :thief :toxic :water-pulse :water-sport :double-edge :icy-wind :mimic]
  :masquerain [:aerial-ace :gust :hyper-beam :nightmare :scary-face :silver-wind :stun-spore :whirlwind :double-edge :icy-wind :mimic :nightmare])

(deflearnset :dp
  :surskit [:agility :attract :baton-pass :blizzard :bubble :bubblebeam :bug-bite :captivate :double-team :endure :facade :flash :foresight :frustration :giga-drain :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mind-reader :mist :mud-shot :mud-slap :natural-gift :protect :psybeam :psych-up :quick-attack :rain-dance :rest :return :secret-power :shadow-ball :signal-beam :sleep-talk :snore :solarbeam :string-shot :substitute :sunny-day :swagger :sweet-scent :swift :thief :toxic :water-pulse :water-sport :double-edge :icy-wind :mimic]
  :masquerain [:aerial-ace :air-cutter :air-slash :bug-buzz :defog :energy-ball :giga-impact :gust :hyper-beam :ominous-wind :roost :scary-face :silver-wind :stun-spore :tailwind :twister :u-turn :whirlwind :double-edge :icy-wind :mimic :nightmare])

(deflearnset :bw
  :surskit [:agility :aqua-jet :attract :baton-pass :blizzard :bubble :bubblebeam :bug-bite :double-team :endure :facade :flash :foresight :frustration :giga-drain :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mind-reader :mist :mud-shot :protect :psybeam :psych-up :quick-attack :rain-dance :rest :return :round :scald :shadow-ball :signal-beam :sleep-talk :snore :solarbeam :struggle-bug :substitute :sunny-day :swagger :sweet-scent :thief :toxic :water-sport :double-edge :icy-wind :mimic]
  :masquerain [:aerial-ace :air-slash :bug-buzz :energy-ball :giga-impact :gust :hyper-beam :ominous-wind :quiver-dance :roost :scary-face :silver-wind :stun-spore :tailwind :u-turn :whirlwind :double-edge :icy-wind :mimic :nightmare])

