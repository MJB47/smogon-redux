
(in-ns 'smogon.dex)


(defpokemon :cottonee
  :name "Cottonee"
  :introduced-in :bw
  :types [[:grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll :infiltrator :prankster]]
  :stats [[40 27 60 37 50 66]]
  :weight [0.6]
  :height [0.3])

(defpokemon :whimsicott
  :name "Whimsicott"
  :introduced-in :bw
  :types [[:grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll :infiltrator :prankster]]
  :stats [[60 67 85 77 75 116]]
  :weight [6.6]
  :height [0.7])

(deffamily (familychain :cottonee :whimsicott))

(deflearnset :bw
  :cottonee [:absorb :attract :beat-up :charm :cotton-guard :cotton-spore :covet :double-team :dream-eater :encore :endeavor :energy-ball :facade :fake-tears :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :helping-hand :hidden-power :knock-off :leech-seed :mega-drain :memento :natural-gift :poisonpowder :protect :razor-leaf :rest :return :round :safeguard :seed-bomb :sleep-talk :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :switcheroo :tailwind :taunt :tickle :toxic :worry-seed]
  :whimsicott [:fling :giga-impact :gust :hurricane :hyper-beam :light-screen :psychic :shadow-ball :thief :trick-room :u-turn])
