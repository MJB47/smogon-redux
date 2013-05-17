
(in-ns 'smogon.dex)


(defpokemon :clamperl
  :name "Clamperl"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:shell-armor]
              :bw [:rattled :shell-armor]]
  :stats [[35 64 85 74 55 32]]
  :weight [52.5]
  :height [0.4])

(defpokemon :huntail
  :name "Huntail"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:swift-swim]
              :bw [:swift-swim :water-veil]]
  :stats [[55 104 105 94 75 52]]
  :weight [27]
  :height [1.7])

(defpokemon :gorebyss
  :name "Gorebyss"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:swift-swim]
              :bw [:hydration :swift-swim]]
  :stats [[55 84 105 114 75 52]]
  :weight [22.6]
  :height [1.8])

(deffamily (familychain :clamperl [:huntail :gorebyss]))

(deflearnset :rs
  :clamperl [:attract :barrier :blizzard :body-slam :clamp :confuse-ray :dive :double-edge :double-team :endure :facade :frustration :hail :hidden-power :ice-beam :icy-wind :iron-defense :mimic :mud-sport :protect :rain-dance :refresh :rest :return :secret-power :sleep-talk :snore :substitute :supersonic :surf :swagger :toxic :water-gun :water-pulse :waterfall :whirlpool]
  :gorebyss [:agility :amnesia :baton-pass :confusion :hydro-pump :hyper-beam :mud-slap :psychic :safeguard :shadow-ball :swift :double-edge :icy-wind :mimic]
  :huntail [:bite :crunch :rock-tomb :scary-face :screech :snatch :double-edge :icy-wind :mimic])

(deflearnset :dp
  :clamperl [:aqua-ring :attract :barrier :blizzard :body-slam :brine :captivate :clamp :confuse-ray :dive :double-team :endure :facade :frustration :hail :hidden-power :ice-beam :icy-wind :iron-defense :mud-sport :muddy-water :natural-gift :protect :rain-dance :refresh :rest :return :secret-power :sleep-talk :snore :substitute :supersonic :surf :swagger :toxic :water-gun :water-pulse :waterfall :whirlpool]
  :gorebyss [:agility :amnesia :aqua-tail :baton-pass :bounce :confusion :giga-impact :hydro-pump :hyper-beam :mud-slap :psych-up :psychic :safeguard :shadow-ball :signal-beam :swift :double-edge :icy-wind :mimic]
  :huntail [:bite :crunch :ice-fang :rock-tomb :scary-face :screech :snatch :sucker-punch :super-fang :double-edge :icy-wind :mimic])

(deflearnset :bw
  :clamperl [:aqua-ring :attract :barrier :blizzard :body-slam :brine :clamp :confuse-ray :dive :double-team :endure :facade :frustration :hail :hidden-power :ice-beam :icy-wind :iron-defense :mud-sport :muddy-water :protect :rain-dance :refresh :rest :return :round :scald :shell-smash :sleep-talk :snore :substitute :supersonic :surf :swagger :toxic :water-gun :water-pulse :waterfall :whirlpool]
  :gorebyss [:agility :amnesia :aqua-tail :baton-pass :bind :bounce :captivate :confusion :giga-impact :hydro-pump :hyper-beam :psych-up :psychic :safeguard :shadow-ball :signal-beam :double-edge :icy-wind :mimic]
  :huntail [:bite :crunch :ice-fang :rock-tomb :scary-face :screech :snatch :super-fang :double-edge :icy-wind :mimic])
