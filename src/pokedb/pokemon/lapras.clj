
(in-ns 'smogon.dex)


(defpokemon :lapras
  :name "Lapras"
  :introduced-in :rb
  :types [[:ice :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:shell-armor :water-absorb]
              :bw [:hydration :shell-armor :water-absorb]]
  :stats [[130 85 80 95 60]
          :gs [130 85 80 85 95 60]]
  :weight [220]
  :height [2.5])

(deffamily (familychain :lapras))

(deflearnset :rb
  :lapras [:bide :blizzard :body-slam :bubblebeam :confuse-ray :double-edge :double-team :dragon-rage :growl :horn-drill :hydro-pump :hyper-beam :ice-beam :mimic :mist :psychic :psywave :rage :reflect :rest :sing :skull-bash :solarbeam :strength :substitute :surf :take-down :thunder :thunderbolt :toxic :water-gun])

(deflearnset :gs
  :lapras [:attract :aurora-beam :blizzard :body-slam :confuse-ray :curse :double-team :dragonbreath :dream-eater :endure :foresight :frustration :growl :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :mist :nightmare :perish-song :protect :psychic :rain-dance :rest :return :rock-smash :safeguard :sing :sleep-talk :snore :strength :surf :swagger :thunder :thunderbolt :toxic :water-gun :whirlpool :zap-cannon])

(deflearnset :rs
  :lapras [:attract :blizzard :body-slam :confuse-ray :curse :dive :double-edge :double-team :dragon-dance :dream-eater :endure :facade :foresight :frustration :growl :hail :hidden-power :horn-drill :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :mimic :mist :nightmare :perish-song :protect :psychic :rain-dance :refresh :rest :return :roar :rock-smash :safeguard :secret-power :sheer-cold :shock-wave :sing :sleep-talk :snore :strength :substitute :surf :swagger :thunder :thunderbolt :tickle :toxic :water-gun :water-pulse :waterfall :double-edge :heal-bell :icy-wind :mimic :nightmare])

(deflearnset :dp
  :lapras [:ancientpower :aqua-tail :attract :avalanche :blizzard :block :body-slam :brine :captivate :confuse-ray :curse :dive :double-team :dragon-dance :dragon-pulse :dream-eater :endure :facade :fissure :foresight :frustration :giga-impact :growl :hail :headbutt :heal-bell :hidden-power :horn-drill :hydro-pump :hyper-beam :ice-beam :ice-shard :icy-wind :iron-head :iron-tail :mist :natural-gift :outrage :perish-song :protect :psychic :rain-dance :refresh :rest :return :roar :rock-smash :safeguard :secret-power :sheer-cold :shock-wave :signal-beam :sing :sleep-talk :snore :strength :substitute :surf :swagger :thunder :thunderbolt :tickle :toxic :water-gun :water-pulse :waterfall :whirlpool :zen-headbutt :double-edge :heal-bell :icy-wind :mimic :nightmare])

(deflearnset :bw
  :lapras [:ancientpower :aqua-tail :attract :avalanche :blizzard :block :body-slam :brine :bulldoze :confuse-ray :curse :dive :double-team :dragon-dance :dragon-pulse :dream-eater :drill-run :echoed-voice :facade :fissure :foresight :frost-breath :frustration :future-sight :giga-impact :growl :hail :heal-bell :hidden-power :horn-drill :hydro-pump :hyper-beam :hyper-voice :ice-beam :ice-shard :icy-wind :iron-head :iron-tail :mist :outrage :perish-song :protect :psychic :rain-dance :refresh :rest :return :roar :rock-smash :round :safeguard :sheer-cold :signal-beam :sing :sleep-talk :snore :strength :substitute :surf :swagger :thunder :thunderbolt :tickle :toxic :water-gun :water-pulse :waterfall :whirlpool :zen-headbutt :double-edge :heal-bell :icy-wind :mimic :nightmare])
