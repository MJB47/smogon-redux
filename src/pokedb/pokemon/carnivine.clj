
(in-ns 'smogon.dex)


(defpokemon :carnivine
  :name "Carnivine"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:levitate]]
  :stats [[74 100 72 90 72 46]]
  :weight [27]
  :height [1.4])

(deffamily (familychain :carnivine))

(deflearnset :dp
  :carnivine [:attract :bind :bite :bug-bite :bullet-seed :captivate :crunch :cut :double-team :endure :energy-ball :facade :faint-attack :flash :fling :frustration :fury-cutter :gastro-acid :giga-drain :giga-impact :grass-knot :growth :hidden-power :hyper-beam :ingrain :knock-off :leech-seed :magical-leaf :mud-slap :natural-gift :payback :power-whip :protect :razor-leaf :rest :return :secret-power :seed-bomb :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stockpile :stun-spore :substitute :sunny-day :swagger :swallow :sweet-scent :swords-dance :synthesis :thief :toxic :vine-whip :worry-seed :wring-out])

(deflearnset :bw
  :carnivine [:attract :bind :bite :bug-bite :crunch :cut :double-team :energy-ball :facade :faint-attack :flash :fling :frustration :gastro-acid :giga-drain :giga-impact :grass-knot :grasswhistle :growth :hidden-power :hyper-beam :ingrain :knock-off :leaf-tornado :leech-seed :magical-leaf :payback :power-whip :protect :rage-powder :razor-leaf :rest :return :round :seed-bomb :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stockpile :stun-spore :substitute :sunny-day :swagger :swallow :sweet-scent :swords-dance :synthesis :thief :toxic :vine-whip :worry-seed :wring-out])
