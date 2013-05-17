
(in-ns 'smogon.dex)


(defpokemon :buizel
  :name "Buizel"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:swift-swim :water-veil]]
  :stats [[55 65 35 60 30 85]]
  :weight [29.5]
  :height [0.7])

(defpokemon :floatzel
  :name "Floatzel"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:swift-swim :water-veil]]
  :stats [[85 105 55 85 50 115]]
  :weight [33.5]
  :height [1.1])

(deffamily (familychain :buizel :floatzel))

(deflearnset :dp
  :buizel [:agility :aqua-jet :attract :baton-pass :blizzard :brick-break :brine :bulk-up :captivate :dig :dive :double-team :doubleslap :endure :facade :focus-punch :frustration :fury-cutter :fury-swipes :growl :hail :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :mud-slap :natural-gift :odor-sleuth :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :rock-smash :rock-tomb :secret-power :slash :sleep-talk :snore :sonicboom :strength :substitute :surf :swagger :swift :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool]
  :floatzel [:aqua-tail :crunch :focus-blast :giga-impact :hyper-beam :ice-fang :low-kick :payback :roar :taunt :torment])

(deflearnset :bw
  :buizel [:agility :aqua-jet :aqua-ring :aqua-tail :attract :baton-pass :blizzard :brick-break :bulk-up :dig :dive :double-hit :double-team :doubleslap :echoed-voice :facade :frustration :fury-cutter :fury-swipes :growl :hail :headbutt :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :me-first :mud-slap :odor-sleuth :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :rock-smash :rock-tomb :round :scald :slash :sleep-talk :snore :sonicboom :strength :substitute :surf :swagger :swift :switcheroo :tail-slap :toxic :water-gun :water-sport :waterfall :whirlpool]
  :floatzel [:crunch :focus-blast :giga-impact :hyper-beam :ice-fang :low-kick :payback :roar :taunt :torment])
