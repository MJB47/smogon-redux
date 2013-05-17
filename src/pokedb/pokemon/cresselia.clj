
(in-ns 'smogon.dex)


(defpokemon :cresselia
  :name "Cresselia"
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:levitate]]
  :stats [[120 70 120 75 130 85]]
  :weight [85.6]
  :height [1.5])

(deffamily (familychain :cresselia))

(deflearnset :dp
  :cresselia [:attract :aurora-beam :calm-mind :captivate :charge-beam :confusion :double-team :dream-eater :endure :energy-ball :facade :flash :frustration :fury-cutter :future-sight :giga-impact :grass-knot :gravity :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :light-screen :lunar-dance :magic-coat :mist :moonlight :mud-slap :natural-gift :protect :psych-up :psychic :psycho-cut :psycho-shift :rain-dance :recycle :reflect :rest :return :safeguard :secret-power :shadow-ball :signal-beam :skill-swap :slash :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :thunder-wave :toxic :trick :trick-room :zen-headbutt])

(deflearnset :bw
  :cresselia [:attract :aurora-beam :calm-mind :charge-beam :confusion :double-team :dream-eater :energy-ball :facade :flash :frustration :future-sight :giga-impact :grass-knot :gravity :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :light-screen :lunar-dance :magic-coat :magic-room :mist :moonlight :protect :psych-up :psychic :psycho-cut :psycho-shift :psyshock :rain-dance :recycle :reflect :rest :return :round :safeguard :shadow-ball :signal-beam :skill-swap :slash :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :telekinesis :thunder-wave :toxic :trick :trick-room :zen-headbutt])
