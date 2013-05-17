
(in-ns 'smogon.dex)

(defpokemon :oddish
  :name "Oddish"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :run-away]]
  :stats [[45 50 55 75 30]
          :gs [45 50 55 75 65 30]]
  :weight [5.4]
  :height [0.5])

(defpokemon :gloom
  :name "Gloom"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :stench]]
  :stats [[60 65 70 85 40]
          :gs [60 65 70 85 75 40]]
  :weight [8.6]
  :height [0.8])

(defpokemon :vileplume
  :name "Vileplume"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :effect-spore]]
  :stats [[75 80 85 100 50]
          :gs [75 80 85 100 90 50]]
  :weight [18.6]
  :height [1.2])

(defpokemon :bellossom
  :name "Bellossom"
  :introduced-in :gs
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :healer]]
  :stats [[75 80 85 90 100 50]]
  :weight [5.8]
  :height [0.4])

(deffamily (familychain :oddish :gloom [:vileplume :bellossom]))

(deflearnset :rb
  :oddish [:absorb :acid :bide :cut :double-edge :double-team :mega-drain :mimic :petal-dance :poisonpowder :rage :reflect :rest :sleep-powder :solarbeam :stun-spore :substitute :swords-dance :take-down :toxic :leech-seed]
  :gloom []
  :vileplume [:body-slam :hyper-beam])

(deflearnset :gs
  :oddish [:absorb :acid :attract :charm :curse :cut :double-team :endure :flail :flash :frustration :giga-drain :hidden-power :moonlight :petal-dance :poisonpowder :protect :razor-leaf :rest :return :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :leech-seed]
  :gloom []
  :vileplume []
  :bellossom [:hyper-beam])

(deflearnset :rs
  :oddish [:absorb :acid :attract :bullet-seed :charm :cut :double-edge :double-team :endure :facade :flail :flash :frustration :giga-drain :hidden-power :ingrain :mimic :moonlight :petal-dance :poisonpowder :protect :razor-leaf :rest :return :secret-power :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :leech-seed :body-slam :double-edge :mimic]
  :gloom [:double-edge :mimic]
  :bellossom [:hyper-beam :magical-leaf :safeguard]
  :vileplume [:aromatherapy :body-slam :mega-drain :body-slam :double-edge :mimic])

(deflearnset :dp
  :oddish [:absorb :acid :attract :bullet-seed :captivate :charm :cut :double-team :endure :energy-ball :facade :flail :flash :frustration :gastro-acid :giga-drain :grass-knot :hidden-power :ingrain :lucky-chant :mega-drain :moonlight :natural-gift :petal-dance :poisonpowder :protect :razor-leaf :rest :return :secret-power :seed-bomb :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :teeter-dance :tickle :toxic :worry-seed :leech-seed :body-slam :double-edge :mimic]
  :gloom [:drain-punch :fling :double-edge :mimic]
  :bellossom [:giga-impact :hyper-beam :leaf-blade :leaf-storm :magical-leaf :safeguard :uproar]
  :vileplume [:aromatherapy :body-slam :double-edge :mimic])

(deflearnset :bw
  :oddish [:absorb :acid :after-you :attract :charm :cut :double-team :energy-ball :facade :flail :flash :frustration :gastro-acid :giga-drain :grass-knot :hidden-power :ingrain :lucky-chant :mega-drain :moonlight :natural-gift :nature-power :petal-dance :poisonpowder :protect :razor-leaf :rest :return :round :secret-power :seed-bomb :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :teeter-dance :tickle :toxic :venoshock :worry-seed :leech-seed :body-slam :double-edge :mimic]
  :gloom [:drain-punch :fling :double-edge :mimic]
  :bellossom [:giga-impact :hyper-beam :leaf-blade :leaf-storm :magical-leaf :safeguard :uproar]
  :vileplume [:aromatherapy :body-slam :double-edge :mimic])
