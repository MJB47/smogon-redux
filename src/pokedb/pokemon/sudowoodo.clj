
(in-ns 'smogon.dex)


(defpokemon :bonsly
  :name "Bonsly"
  :introduced-in :dp
  :types [[:rock]]
  :egggroups [[]]
  :abilities [[:rattled :rock-head :sturdy]]
  :stats [[50 80 95 10 45 10]]
  :weight [15]
  :height [0.5])

(defpokemon :sudowoodo
  :name "Sudowoodo"
  :introduced-in :gs
  :types [[:rock]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rattled :rock-head :sturdy]]
  :stats [[70 100 115 30 65 30]]
  :weight [38]
  :height [1.2])

(deffamily (familychain :bonsly :sudowoodo))



(deflearnset :gs
  :sudowoodo [:attract :curse :defense-curl :dig :double-team :dynamicpunch :earthquake :endure :faint-attack :fire-punch :flail :frustration :headbutt :hidden-power :ice-punch :low-kick :mimic :mud-slap :protect :psych-up :rest :return :rock-slide :rock-smash :rock-throw :rollout :sandstorm :selfdestruct :slam :sleep-talk :snore :strength :sunny-day :swagger :thief :thunderpunch :toxic])

(deflearnset :rs
  :sudowoodo [:attract :block :body-slam :brick-break :calm-mind :counter :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :faint-attack :fire-punch :flail :focus-punch :frustration :hidden-power :ice-punch :low-kick :mega-kick :mega-punch :mimic :mud-slap :protect :psych-up :rest :return :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sandstorm :secret-power :seismic-toss :selfdestruct :slam :sleep-talk :snore :strength :substitute :sunny-day :swagger :taunt :thief :thunderpunch :toxic])

(deflearnset :dp
  :bonsly [:attract :block :brick-break :calm-mind :captivate :copycat :defense-curl :dig :double-edge :double-team :earth-power :endure :explosion :facade :faint-attack :fake-tears :flail :frustration :harden :headbutt :helping-hand :hidden-power :low-kick :mimic :natural-gift :protect :psych-up :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :role-play :rollout :sand-tomb :sandstorm :secret-power :selfdestruct :slam :sleep-talk :snore :stealth-rock :substitute :sucker-punch :sunny-day :swagger :thief :toxic :uproar]
  :sudowoodo [:earthquake :fire-punch :fling :focus-punch :hammer-arm :ice-punch :mud-slap :rock-smash :stone-edge :strength :taunt :thunderpunch :torment :wood-hammer])

(deflearnset :bw
  :bonsly [:after-you :attract :block :brick-break :calm-mind :copycat :covet :curse :defense-curl :dig :double-edge :double-team :earth-power :endure :explosion :facade :faint-attack :fake-tears :flail :foul-play :frustration :harden :headbutt :helping-hand :hidden-power :low-kick :mimic :protect :psych-up :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :role-play :rollout :round :sand-tomb :sandstorm :selfdestruct :slam :sleep-talk :smack-down :snore :stealth-rock :substitute :sucker-punch :sunny-day :swagger :thief :toxic :uproar]
  :sudowoodo [:bulldoze :counter :earthquake :fire-punch :fling :hammer-arm :ice-punch :rock-smash :stone-edge :strength :taunt :thunderpunch :torment :wood-hammer])

