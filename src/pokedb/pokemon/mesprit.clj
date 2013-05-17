
(in-ns 'smogon.dex)


(defpokemon :mesprit
  :name "Mesprit"
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:levitate]]
  :stats [[80 105 105 105 105 80]]
  :weight [0.3]
  :height [0.3])

(deffamily (familychain :mesprit))

(deflearnset :dp
  :mesprit [:blizzard :calm-mind :charge-beam :charm :confusion :copycat :double-team :dream-eater :endure :energy-ball :extrasensory :facade :fire-punch :flash :fling :frustration :future-sight :giga-impact :grass-knot :headbutt :healing-wish :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :imprison :iron-tail :knock-off :light-screen :lucky-chant :magic-coat :mud-slap :natural-gift :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snore :stealth-rock :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :water-pulse :zen-headbutt])

(deflearnset :bw
  :mesprit [:acrobatics :blizzard :calm-mind :charge-beam :charm :confusion :copycat :double-team :dream-eater :energy-ball :extrasensory :facade :fire-punch :flash :fling :frustration :future-sight :giga-impact :grass-knot :healing-wish :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :imprison :iron-tail :knock-off :light-screen :lucky-chant :magic-coat :magic-room :natural-gift :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :stealth-rock :substitute :sunny-day :swagger :swift :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :wonder-room :zen-headbutt])
