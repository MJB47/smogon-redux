
(in-ns 'smogon.dex)


(defpokemon :mudkip
  :name "Mudkip"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:damp :torrent]]
  :stats [[50 70 50 50 50 40]]
  :weight [7.6]
  :height [0.4])

(defpokemon :marshtomp
  :name "Marshtomp"
  :introduced-in :rs
  :types [[:ground :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:damp :torrent]]
  :stats [[70 85 70 60 70 50]]
  :weight [28]
  :height [0.7])

(defpokemon :swampert
  :name "Swampert"
  :introduced-in :rs
  :types [[:ground :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:damp :torrent]]
  :stats [[100 110 90 85 90 60]]
  :weight [81.9]
  :height [1.5])

(deffamily (familychain :mudkip :marshtomp :swampert))

(deflearnset :rs
  :mudkip [:attract :bide :blizzard :body-slam :curse :defense-curl :dig :dive :double-edge :double-team :endeavor :endure :facade :foresight :frustration :growl :hail :hidden-power :hydro-pump :ice-ball :ice-beam :icy-wind :iron-tail :mimic :mirror-coat :mud-slap :mud-sport :protect :rain-dance :refresh :rest :return :rock-smash :rock-tomb :rollout :secret-power :sleep-talk :snore :stomp :strength :substitute :surf :swagger :tackle :take-down :toxic :uproar :water-gun :water-pulse :waterfall :whirlpool :body-slam :icy-wind :mimic]
  :marshtomp [:counter :dynamicpunch :earthquake :ice-punch :mega-kick :mega-punch :mud-shot :muddy-water :rock-slide :seismic-toss :body-slam :icy-wind :mimic :seismic-toss]
  :swampert [:brick-break :focus-punch :hyper-beam :roar :body-slam :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :mudkip [:ancientpower :aqua-tail :attract :bide :bite :blizzard :captivate :counter :curse :dig :dive :double-edge :double-team :earth-power :endeavor :endure :facade :foresight :frustration :growl :hail :headbutt :hidden-power :hydro-pump :ice-ball :ice-beam :icy-wind :iron-tail :low-kick :mirror-coat :mud-bomb :mud-slap :mud-sport :natural-gift :protect :rain-dance :refresh :rest :return :rock-slide :rock-smash :rock-tomb :rollout :secret-power :sleep-talk :sludge :snore :stomp :strength :substitute :superpower :surf :swagger :tackle :take-down :toxic :uproar :water-gun :water-pulse :waterfall :whirlpool :yawn :body-slam :icy-wind :mimic]
  :marshtomp [:brick-break :earthquake :fling :ice-punch :mud-shot :muddy-water :stealth-rock :body-slam :icy-wind :mimic :seismic-toss]
  :swampert [:avalanche :focus-blast :focus-punch :giga-impact :hammer-arm :hydro-cannon :hyper-beam :outrage :roar :rock-climb :stone-edge :body-slam :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :mudkip [:ancientpower :aqua-tail :attract :avalanche :bide :bite :blizzard :counter :curse :dig :dive :double-edge :double-team :earth-power :echoed-voice :endeavor :facade :foresight :frustration :growl :hail :hidden-power :hydro-pump :ice-ball :ice-beam :icy-wind :iron-tail :low-kick :mirror-coat :mud-bomb :mud-slap :mud-sport :protect :rain-dance :refresh :rest :return :rock-slide :rock-smash :rock-tomb :round :scald :sleep-talk :sludge :sludge-wave :snore :stomp :strength :substitute :superpower :surf :swagger :tackle :take-down :toxic :uproar :water-gun :water-pledge :waterfall :whirlpool :wide-guard :yawn :body-slam :icy-wind :mimic]
  :marshtomp [:brick-break :bulldoze :earthquake :fling :ice-punch :mud-shot :muddy-water :stealth-rock :body-slam :icy-wind :mimic :seismic-toss]
  :swampert [:focus-blast :giga-impact :hammer-arm :hydro-cannon :hyper-beam :outrage :roar :stone-edge :body-slam :icy-wind :mimic :seismic-toss])
