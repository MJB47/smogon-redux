
(in-ns 'smogon.dex)


(defpokemon :yanma
  :name "Yanma"
  :introduced-in :gs
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:compoundeyes :speed-boost]
              :bw [:compoundeyes :frisk :speed-boost]]
  :stats [[65 65 45 75 45 95]]
  :weight [38]
  :height [1.2])

(defpokemon :yanmega
  :name "Yanmega"
  :introduced-in :dp
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:frisk :speed-boost :tinted-lens]]
  :stats [[86 76 86 116 56 95]]
  :weight [51.5]
  :height [1.9])

(deffamily (familychain :yanma :yanmega))



(deflearnset :gs
  :yanma [:attract :curse :detect :double-team :endure :flash :foresight :frustration :giga-drain :headbutt :hidden-power :leech-life :protect :quick-attack :rest :return :reversal :screech :sleep-talk :snore :solarbeam :sonicboom :sunny-day :supersonic :swagger :swift :tackle :thief :toxic :whirlwind :wing-attack])

(deflearnset :rs
  :yanma [:aerial-ace :attract :detect :double-edge :double-team :dream-eater :endure :facade :flash :foresight :frustration :giga-drain :hidden-power :hypnosis :leech-life :mimic :protect :psychic :quick-attack :rest :return :reversal :screech :secret-power :shadow-ball :signal-beam :silver-wind :sleep-talk :snore :solarbeam :sonicboom :steel-wing :substitute :sunny-day :supersonic :swagger :swift :tackle :thief :toxic :uproar :whirlwind :wing-attack :mimic])

(deflearnset :dp
  :yanma [:aerial-ace :air-cutter :air-slash :ancientpower :attract :bug-bite :bug-buzz :captivate :defog :detect :double-team :dream-eater :endure :facade :faint-attack :feint :flash :foresight :frustration :giga-drain :headbutt :hidden-power :hypnosis :leech-life :natural-gift :ominous-wind :protect :psychic :pursuit :quick-attack :rest :return :reversal :roost :screech :secret-power :shadow-ball :signal-beam :silver-wind :sleep-talk :snore :solarbeam :sonicboom :steel-wing :string-shot :substitute :sunny-day :supersonic :swagger :swift :tackle :tailwind :thief :toxic :u-turn :uproar :whirlwind :wing-attack :mimic]
  :yanmega [:giga-impact :hyper-beam :mud-slap :night-slash :psych-up :slash])

(deflearnset :bw
  :yanma [:aerial-ace :air-slash :ancientpower :attract :bug-bite :bug-buzz :detect :double-edge :double-team :dream-eater :facade :faint-attack :feint :flash :foresight :frustration :giga-drain :hidden-power :hypnosis :leech-life :protect :psychic :pursuit :quick-attack :rest :return :reversal :roost :round :screech :secret-power :shadow-ball :signal-beam :silver-wind :sleep-talk :snore :solarbeam :sonicboom :substitute :sunny-day :supersonic :swagger :tackle :tailwind :thief :toxic :u-turn :uproar :whirlwind :wing-attack :mimic]
  :yanmega [:giga-impact :hyper-beam :night-slash :psych-up :slash :struggle-bug])

