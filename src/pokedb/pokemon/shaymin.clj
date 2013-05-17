
(in-ns 'smogon.dex)


(defpokemon :shaymin
  :name "Shaymin"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[]]
  :abilities [[:natural-cure]]
  :stats [[100 100 100 100 100 100]]
  :weight [2.1]
  :height [0.2])

(defpokemon :shaymin-s
  :name "Shaymin-Sky"
  :introduced-in :dp
  :types [[:flying :grass]]
  :egggroups [[]]
  :abilities [[:serene-grace]]
  :stats [[100 103 75 120 75 127]]
  :weight [5.2]
  :height [0.4])

(deffamily (familychain :shaymin :shaymin-s))

(deflearnset :dp
  :shaymin [:aromatherapy :bullet-seed :defense-curl :double-team :earth-power :endeavor :endure :energy-ball :facade :flash :frustration :giga-drain :giga-impact :grass-knot :grasswhistle :growth :headbutt :healing-wish :hidden-power :hyper-beam :last-resort :leech-seed :lucky-chant :magical-leaf :mud-slap :natural-gift :protect :psych-up :psychic :rest :return :safeguard :secret-power :seed-bomb :seed-flare :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :sweet-scent :swift :swords-dance :synthesis :toxic :worry-seed :zen-headbutt])

(deflearnset :bw
  :shaymin [:aromatherapy :covet :double-team :earth-power :endeavor :energy-ball :facade :flash :frustration :giga-drain :giga-impact :grass-knot :growth :healing-wish :hidden-power :hyper-beam :last-resort :leech-seed :magical-leaf :natural-gift :protect :psych-up :psychic :rest :return :round :safeguard :seed-bomb :seed-flare :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :sweet-scent :swords-dance :synthesis :toxic :worry-seed :zen-headbutt])
