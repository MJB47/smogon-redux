
(in-ns 'smogon.dex)


(defpokemon :foongus
  :name "Foongus"
  :introduced-in :bw
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:effect-spore :regenerator]]
  :stats [[69 55 45 55 55 15]]
  :weight [1]
  :height [0.2])

(defpokemon :amoonguss
  :name "Amoonguss"
  :introduced-in :bw
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:effect-spore :regenerator]]
  :stats [[114 85 70 85 80 30]]
  :weight [10.5]
  :height [0.6])

(deffamily (familychain :foongus :amoonguss))

(deflearnset :bw
  :foongus [:absorb :after-you :astonish :attract :bide :body-slam :clear-smog :defense-curl :double-team :endure :energy-ball :facade :faint-attack :flash :foul-play :frustration :gastro-acid :giga-drain :grass-knot :growth :hidden-power :ingrain :mega-drain :payback :poisonpowder :protect :rage-powder :rain-dance :rest :return :rollout :round :seed-bomb :sleep-talk :sludge-bomb :snore :solarbeam :spore :stun-spore :substitute :sunny-day :swagger :sweet-scent :synthesis :toxic :venoshock :worry-seed]
  :amoonguss [:giga-impact :hyper-beam])
