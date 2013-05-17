
(in-ns 'smogon.dex)


(defpokemon :snover
  :name "Snover"
  :introduced-in :dp
  :types [[:grass :ice]]
  :egggroups [[:monster :plant]]
  :abilities [[:snow-warning :soundproof]]
  :stats [[60 62 50 62 60 40]]
  :weight [50.5]
  :height [1])

(defpokemon :abomasnow
  :name "Abomasnow"
  :introduced-in :dp
  :types [[:grass :ice]]
  :egggroups [[:monster :plant]]
  :abilities [[:snow-warning :soundproof]]
  :stats [[90 92 75 92 85 60]]
  :weight [135.5]
  :height [2.2])

(deffamily (familychain :snover :abomasnow))



(deflearnset :dp
  :snover [:attract :avalanche :blizzard :bullet-seed :captivate :double-edge :double-team :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :hail :headbutt :hidden-power :ice-beam :ice-punch :ice-shard :icy-wind :ingrain :iron-tail :leech-seed :leer :light-screen :magical-leaf :mist :mud-slap :natural-gift :powder-snow :protect :rain-dance :razor-leaf :rest :return :role-play :safeguard :secret-power :seed-bomb :shadow-ball :sheer-cold :skull-bash :sleep-talk :snore :solarbeam :stomp :substitute :swagger :swords-dance :synthesis :toxic :water-pulse :wood-hammer :worry-seed]
  :abomasnow [:block :brick-break :earthquake :fling :focus-blast :focus-punch :giga-impact :hyper-beam :outrage :rock-climb :rock-slide :rock-smash :rock-tomb :strength])

(deflearnset :bw
  :snover [:attract :avalanche :blizzard :bullet-seed :double-edge :double-team :energy-ball :facade :flash :frost-breath :frustration :giga-drain :grass-knot :grasswhistle :growth :hail :hidden-power :ice-beam :ice-punch :ice-shard :icy-wind :ingrain :iron-tail :leech-seed :leer :light-screen :magical-leaf :mist :natural-gift :powder-snow :protect :rain-dance :razor-leaf :rest :return :role-play :round :safeguard :seed-bomb :shadow-ball :sheer-cold :skull-bash :sleep-talk :snore :solarbeam :stomp :substitute :swagger :swords-dance :synthesis :toxic :wood-hammer :worry-seed]
  :abomasnow [:block :brick-break :bulldoze :earthquake :fling :focus-blast :giga-impact :hyper-beam :outrage :rock-slide :rock-smash :rock-tomb :strength])

