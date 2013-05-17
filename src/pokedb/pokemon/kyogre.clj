
(in-ns 'smogon.dex)


(defpokemon :kyogre
  :name "Kyogre"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[]]
  :abilities [[:drizzle]
              :bw [:drizzle]]
  :stats [[100 100 90 150 140 90]]
  :weight [352]
  :height [4.5])

(deffamily (familychain :kyogre))

(deflearnset :rs
  :kyogre [:ancientpower :blizzard :body-slam :brick-break :calm-mind :defense-curl :dive :double-edge :double-team :earthquake :endure :facade :frustration :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :mimic :mud-slap :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :safeguard :scary-face :secret-power :sheer-cold :shock-wave :sleep-talk :snore :strength :substitute :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :water-pulse :water-spout :waterfall :icy-wind :mimic])

(deflearnset :dp
  :kyogre [:ancientpower :aqua-ring :aqua-tail :avalanche :blizzard :block :body-slam :brick-break :brine :calm-mind :dive :double-edge :double-team :earthquake :endure :facade :frustration :giga-impact :hail :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-head :mud-slap :muddy-water :natural-gift :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :safeguard :scary-face :secret-power :sheer-cold :shock-wave :signal-beam :sleep-talk :snore :strength :substitute :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :uproar :water-pulse :water-spout :waterfall :whirlpool :icy-wind :mimic])

(deflearnset :bw
  :kyogre [:ancientpower :aqua-ring :aqua-tail :blizzard :block :body-slam :brick-break :bulldoze :calm-mind :dive :double-edge :double-team :earthquake :facade :frustration :giga-impact :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-head :muddy-water :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :round :safeguard :scald :scary-face :sheer-cold :signal-beam :sleep-talk :snore :strength :substitute :surf :swagger :thunder :thunder-wave :thunderbolt :toxic :uproar :water-pulse :water-spout :waterfall :icy-wind :mimic])
