
(in-ns 'smogon.dex)


(defpokemon :castform
  :name "Castform"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:fairy :indeterminate]]
  :abilities [[:forecast]
              :bw [:forecast]]
  :stats [[70 70 70 70 70 70]]
  :weight [0.8]
  :height [0.3])

(deffamily (familychain :castform))

(deflearnset :rs
  :castform [:attract :blizzard :body-slam :defense-curl :double-edge :double-team :ember :endure :facade :fire-blast :flamethrower :flash :frustration :future-sight :hail :hidden-power :ice-beam :icy-wind :mimic :powder-snow :protect :psych-up :rain-dance :rest :return :sandstorm :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :thief :thunder :thunder-wave :thunderbolt :toxic :water-gun :water-pulse :weather-ball])

(deflearnset :dp
  :castform [:amnesia :attract :avalanche :blizzard :captivate :disable :double-team :ember :endure :energy-ball :facade :fire-blast :flamethrower :flash :frustration :future-sight :hail :hidden-power :ice-beam :icy-wind :last-resort :lucky-chant :natural-gift :ominous-wind :powder-snow :protect :psych-up :rain-dance :rest :return :sandstorm :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :tailwind :thief :thunder :thunder-wave :thunderbolt :toxic :water-gun :water-pulse :weather-ball])

(deflearnset :bw
  :castform [:amnesia :attract :blizzard :clear-smog :disable :double-team :ember :energy-ball :facade :fire-blast :flamethrower :flash :frustration :future-sight :hail :headbutt :hex :hidden-power :hydro-pump :ice-beam :icy-wind :incinerate :last-resort :lucky-chant :ominous-wind :powder-snow :protect :psych-up :rain-dance :rest :retaliate :return :round :sandstorm :scald :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :tackle :tailwind :thief :thunder :thunder-wave :thunderbolt :toxic :water-gun :weather-ball :work-up])
