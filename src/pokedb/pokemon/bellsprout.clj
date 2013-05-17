
(in-ns 'smogon.dex)

(defpokemon :bellsprout
  :name "Bellsprout"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :gluttony]]
  :stats [[50 75 35 70 40]
          :gs [50 75 35 70 30 40]]
  :weight [4]
  :height [0.7])

(defpokemon :weepinbell
  :name "Weepinbell"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :gluttony]]
  :stats [[65 90 50 85 55]
          :gs [65 90 50 85 45 55]]
  :weight [6.4]
  :height [1])

(defpokemon :victreebel
  :name "Victreebel"
  :introduced-in :rb
  :types [[:grass :poison]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :gluttony]]
  :stats [[80 105 65 100 70]
          :gs [80 105 65 100 60 70]]
  :weight [15.5]
  :height [1.7])

(deffamily (familychain :bellsprout :weepinbell :victreebel))

(deflearnset :rb
  :bellsprout [:acid :bide :cut :double-edge :double-team :growth :mega-drain :mimic :poisonpowder :rage :razor-leaf :reflect :rest :slam :sleep-powder :solarbeam :stun-spore :substitute :swords-dance :take-down :toxic :vine-whip :wrap :lovely-kiss :sweet-kiss]
  :weepinbell []
  :victreebel [:body-slam :hyper-beam])

(deflearnset :gs
  :bellsprout [:acid :attract :curse :cut :double-team :encore :endure :flash :frustration :giga-drain :growth :hidden-power :leech-life :poisonpowder :protect :razor-leaf :reflect :rest :return :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :sunny-day :swagger :sweet-scent :swords-dance :synthesis :toxic :vine-whip :wrap :lovely-kiss :sweet-kiss]
  :weepinbell []
  :victreebel [:hyper-beam])

(deflearnset :rs
  :bellsprout [:acid :attract :bullet-seed :cut :double-edge :double-team :encore :endure :facade :flash :frustration :giga-drain :growth :hidden-power :ingrain :leech-life :magical-leaf :mimic :poisonpowder :protect :razor-leaf :reflect :rest :return :secret-power :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :thief :toxic :vine-whip :wrap :teeter-dance]
  :weepinbell [ :double-edge :mimic :morning-sun]
  :victreebel [:body-slam :hyper-beam :spit-up :stockpile :swallow :body-slam :double-edge :mimic])

(deflearnset :dp
  :bellsprout [:acid :attract :bullet-seed :captivate :cut :double-team :encore :endure :energy-ball :facade :flash :frustration :gastro-acid :giga-drain :grass-knot :growth :hidden-power :ingrain :knock-off :leech-life :magical-leaf :natural-gift :poisonpowder :protect :razor-leaf :reflect :rest :return :secret-power :seed-bomb :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sucker-punch :sunny-day :swagger :sweet-scent :swords-dance :synthesis :thief :tickle :toxic :vine-whip :weather-ball :worry-seed :wrap :wring-out :teeter-dance]
  :weepinbell [ :double-edge :mimic :morning-sun]
  :victreebel [:giga-impact :hyper-beam :leaf-blade :leaf-storm :spit-up :stockpile :swallow :body-slam :double-edge :mimic])

(deflearnset :bw
  :bellsprout [:acid :attract :bind :body-slam :bullet-seed :clear-smog :cut :double-team :encore :energy-ball :facade :flash :frustration :gastro-acid :giga-drain :grass-knot :growth :hidden-power :ingrain :knock-off :leech-life :magical-leaf :mimic :natural-gift :poisonpowder :power-whip :protect :razor-leaf :reflect :rest :return :round :seed-bomb :slam :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :thief :tickle :toxic :venoshock :vine-whip :weather-ball :worry-seed :wrap :wring-out :teeter-dance]
  :weepinbell [:bug-bite :morning-sun :double-edge :double-edge :mimic :morning-sun]
  :victreebel [:giga-impact :hyper-beam :leaf-blade :leaf-storm :leaf-tornado :spit-up :stockpile :swallow :body-slam :double-edge :mimic])
