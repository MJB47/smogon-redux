
(in-ns 'smogon.dex)

(defpokemon :celebi
  :name "Celebi"
  :introduced-in :gs
  :types [[:grass :psychic]]
  :egggroups [[]]
  :abilities [[:natural-cure]
              :bw [:natural-cure]]
  :stats [[100 100 100 100 100 100]]
  :weight [5]
  :height [0.6])

(deffamily (familychain :celebi))

(deflearnset :gs
  :celebi [:ancientpower :baton-pass :confusion :curse :defense-curl :detect :double-team :dream-eater :endure :flash :frustration :future-sight :giga-drain :heal-bell :hidden-power :hyper-beam :leech-seed :mud-slap :nightmare :perish-song :protect :psych-up :psychic :rain-dance :recover :rest :return :safeguard :sandstorm :shadow-ball :sleep-talk :snore :solarbeam :sunny-day :swagger :sweet-scent :swift :toxic])

(deflearnset :rs
  :celebi [:aerial-ace :ancientpower :baton-pass :body-slam :calm-mind :confusion :cut :defense-curl :double-edge :double-team :dream-eater :endure :facade :flash :frustration :future-sight :giga-drain :heal-bell :hidden-power :hyper-beam :leech-seed :light-screen :metronome :mimic :mud-slap :nightmare :perish-song :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :swords-dance :toxic :water-pulse])

(deflearnset :dp
  :celebi [:aerial-ace :ancientpower :baton-pass :body-slam :calm-mind :charge-beam :confusion :cut :double-team :dream-eater :earth-power :endure :energy-ball :facade :flash :fling :frustration :future-sight :giga-drain :giga-impact :grass-knot :heal-bell :heal-block :healing-wish :helping-hand :hidden-power :hyper-beam :last-resort :leaf-storm :leech-seed :light-screen :magic-coat :magical-leaf :mimic :mud-slap :natural-gift :perish-song :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :safeguard :sandstorm :secret-power :seed-bomb :shadow-ball :shock-wave :signal-beam :silver-wind :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :substitute :sucker-punch :sunny-day :swagger :swift :swords-dance :synthesis :thunder-wave :toxic :trick :trick-room :u-turn :uproar :water-pulse :worry-seed :zen-headbutt])

(deflearnset :bw
  :celebi [:aerial-ace :ancientpower :baton-pass :body-slam :calm-mind :charge-beam :confusion :cut :double-team :dream-eater :earth-power :echoed-voice :energy-ball :facade :flash :fling :frustration :future-sight :giga-drain :giga-impact :grass-knot :heal-bell :heal-block :healing-wish :helping-hand :hidden-power :hyper-beam :last-resort :leaf-storm :leech-seed :light-screen :magic-coat :magic-room :magical-leaf :mimic :natural-gift :perish-song :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :round :safeguard :sandstorm :seed-bomb :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :substitute :sunny-day :swagger :swords-dance :synthesis :thunder-wave :toxic :trick :trick-room :u-turn :uproar :wonder-room :worry-seed :zen-headbutt])
