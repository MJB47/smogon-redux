
(in-ns 'smogon.dex)


(defpokemon :bidoof
  :name "Bidoof"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground :water-1]]
  :abilities [[:moody :simple :unaware]]
  :stats [[59 45 40 35 40 31]]
  :weight [20]
  :height [0.5])

(defpokemon :bibarel
  :name "Bibarel"
  :introduced-in :dp
  :types [[:normal :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:moody :simple :unaware]]
  :stats [[79 85 60 55 60 71]]
  :weight [31.5]
  :height [1])


(deffamily (familychain :bidoof :bibarel))

(deflearnset :dp
  :bidoof [:amnesia :aqua-tail :attract :blizzard :captivate :charge-beam :curse :cut :defense-curl :dig :double-edge :double-team :endure :facade :frustration :fury-cutter :fury-swipes :grass-knot :growl :headbutt :hidden-power :hyper-fang :ice-beam :icy-wind :iron-tail :last-resort :mud-slap :natural-gift :odor-sleuth :pluck :protect :quick-attack :rain-dance :rest :return :rock-smash :rollout :secret-power :shadow-ball :shock-wave :sleep-talk :snore :stealth-rock :substitute :sunny-day :super-fang :superpower :swagger :swift :tackle :take-down :taunt :thief :thunder :thunder-wave :thunderbolt :toxic :water-sport :yawn]
  :bibarel [:dive :fling :focus-punch :giga-impact :hyper-beam :rock-climb :strength :surf :water-gun :water-pulse :waterfall :whirlpool])

(deflearnset :bw
  :bidoof [:amnesia :aqua-tail :attract :blizzard :charge-beam :covet :curse :cut :defense-curl :dig :double-edge :double-team :echoed-voice :endure :facade :frustration :fury-swipes :grass-knot :growl :headbutt :hidden-power :hyper-fang :ice-beam :icy-wind :iron-tail :last-resort :odor-sleuth :pluck :protect :quick-attack :rain-dance :rest :retaliate :return :rock-climb :rock-smash :rollout :round :shadow-ball :skull-bash :sleep-talk :snore :stealth-rock :substitute :sunny-day :super-fang :superpower :swagger :tackle :take-down :taunt :thief :thunder :thunder-wave :thunderbolt :toxic :water-sport :work-up :yawn]
  :bibarel [:bulldoze :dive :fling :giga-impact :hyper-beam :scald :strength :surf :water-gun :waterfall])
