
(in-ns 'smogon.dex)


(defpokemon :exeggcute
  :name "Exeggcute"
  :introduced-in :rb
  :types [[:grass :psychic]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :harvest]]
  :stats [[60 40 80 60 40]
          :gs [60 40 80 60 45 40]]
  :weight [2.5]
  :height [0.4])

(defpokemon :exeggutor
  :name "Exeggutor"
  :introduced-in :rb
  :types [[:grass :psychic]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll]
              :bw [:chlorophyll :harvest]]
  :stats [[95 95 85 125 55]
          :gs [95 95 85 125 65 55]]
  :weight [120]
  :height [2])

(deffamily (familychain :exeggcute :exeggutor))

(deflearnset :rb
  :exeggcute [:barrage :bide :double-edge :double-team :egg-bomb :explosion :hypnosis :leech-seed :mimic :poisonpowder :psychic :psywave :rage :reflect :rest :selfdestruct :sleep-powder :solarbeam :stun-spore :substitute :take-down :teleport :toxic]
  :exeggutor [:hyper-beam :mega-drain :stomp :strength])

(deflearnset :gs
  :exeggcute [:ancientpower :attract :barrage :confusion :curse :double-team :dream-eater :endure :flash :frustration :giga-drain :hidden-power :hypnosis :leech-seed :mega-drain :moonlight :nightmare :poisonpowder :protect :psych-up :psychic :reflect :rest :return :rollout :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :strength :stun-spore :sunny-day :swagger :synthesis :thief :toxic]
  :exeggutor [:egg-bomb :headbutt :hyper-beam :stomp])

(deflearnset :rs
  :exeggcute [:ancientpower :attract :barrage :bullet-seed :confusion :curse :double-edge :double-team :dream-eater :endure :explosion :facade :flash :frustration :giga-drain :hidden-power :hypnosis :ingrain :leech-seed :light-screen :mimic :moonlight :nightmare :poisonpowder :protect :psych-up :psychic :reflect :rest :return :rollout :secret-power :selfdestruct :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :strength :stun-spore :substitute :sunny-day :swagger :synthesis :thief :toxic :uproar :double-edge :mimic :nightmare :selfdestruct :swagger]
  :exeggutor [:egg-bomb :hyper-beam :stomp :double-edge :mimic :nightmare :refresh :selfdestruct :swagger])

(deflearnset :dp
  :exeggcute [:ancientpower :attract :barrage :bullet-seed :captivate :confusion :curse :double-team :dream-eater :endure :energy-ball :explosion :facade :flash :frustration :giga-drain :grass-knot :gravity :hidden-power :hypnosis :ingrain :leaf-storm :leech-seed :light-screen :lucky-chant :moonlight :natural-gift :nature-power :poisonpowder :power-swap :protect :psych-up :psychic :reflect :rest :return :rollout :secret-power :seed-bomb :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :strength :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :thief :toxic :trick-room :uproar :worry-seed :double-edge :mimic :nightmare :selfdestruct :swagger]
  :exeggutor [:egg-bomb :giga-impact :headbutt :hyper-beam :low-kick :stomp :wood-hammer :zen-headbutt :double-edge :mimic :nightmare :refresh :selfdestruct :swagger])

(deflearnset :bw
  :exeggcute [:ancientpower :attract :barrage :bestow :block :bullet-seed :confusion :curse :double-team :dream-eater :energy-ball :explosion :extrasensory :facade :flash :frustration :giga-drain :grass-knot :gravity :hidden-power :hypnosis :ingrain :leaf-storm :leech-seed :light-screen :lucky-chant :moonlight :natural-gift :nature-power :poisonpowder :power-swap :protect :psych-up :psychic :reflect :rest :return :round :seed-bomb :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :strength :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :telekinesis :thief :toxic :trick-room :uproar :worry-seed :double-edge :mimic :nightmare :selfdestruct :swagger]
  :exeggutor [:egg-bomb :giga-impact :hyper-beam :low-kick :psyshock :stomp :wood-hammer :zen-headbutt :double-edge :mimic :nightmare :refresh :selfdestruct :swagger])
