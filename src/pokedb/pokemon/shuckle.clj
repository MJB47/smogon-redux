
(in-ns 'smogon.dex)


(defpokemon :shuckle
  :name "Shuckle"
  :introduced-in :gs
  :types [[:bug :rock]]
  :egggroups [[:bug]]
  :abilities [[:sturdy]
              :dp [:gluttony :sturdy]
              :bw [:contrary :gluttony :sturdy]]
  :stats [[20 10 230 10 230 5]]
  :weight [20.5]
  :height [0.6])

(deffamily (familychain :shuckle))

(deflearnset :gs
  :shuckle [:attract :bide :constrict :curse :defense-curl :dig :double-team :earthquake :encore :endure :flash :frustration :headbutt :hidden-power :mud-slap :protect :rest :return :rock-smash :rollout :safeguard :sandstorm :sleep-talk :sludge-bomb :snore :strength :sunny-day :swagger :sweet-scent :toxic :withdraw :wrap])

(deflearnset :rs
  :shuckle [:attract :bide :body-slam :constrict :defense-curl :dig :double-edge :double-team :earthquake :encore :endure :facade :flash :frustration :hidden-power :mimic :mud-slap :protect :rest :return :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :secret-power :sleep-talk :sludge-bomb :snore :strength :substitute :sunny-day :swagger :sweet-scent :toxic :withdraw :wrap :body-slam :double-edge :mimic])

(deflearnset :dp
  :shuckle [:acupressure :ancientpower :attract :bide :bug-bite :captivate :constrict :dig :double-team :earth-power :earthquake :encore :endure :facade :flash :frustration :gastro-acid :gyro-ball :headbutt :helping-hand :hidden-power :knock-off :mud-slap :natural-gift :power-trick :protect :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sand-tomb :sandstorm :secret-power :sleep-talk :sludge-bomb :snore :stealth-rock :stone-edge :strength :string-shot :substitute :sunny-day :swagger :sweet-scent :toxic :withdraw :wrap :body-slam :double-edge :mimic])

(deflearnset :bw
  :shuckle [:acid :acupressure :after-you :attract :bide :bind :bug-bite :bulldoze :constrict :dig :double-team :earth-power :earthquake :encore :facade :final-gambit :flash :frustration :gastro-acid :guard-split :gyro-ball :helping-hand :hidden-power :knock-off :mud-slap :power-split :power-trick :protect :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :safeguard :sand-tomb :sandstorm :shell-smash :sleep-talk :sludge-bomb :sludge-wave :smack-down :snore :stealth-rock :stone-edge :strength :struggle-bug :substitute :sunny-day :swagger :sweet-scent :toxic :venoshock :withdraw :wrap :body-slam :double-edge :mimic])
