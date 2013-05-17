
(in-ns 'smogon.dex)


(defpokemon :sentret
  :name "Sentret"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:keen-eye :run-away]
              :bw [:frisk :keen-eye :run-away]]
  :stats [[35 46 34 35 45 20]]
  :weight [6]
  :height [0.8])

(defpokemon :furret
  :name "Furret"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:keen-eye :run-away]
              :bw [:frisk :keen-eye :run-away]]
  :stats [[85 76 64 45 55 90]]
  :weight [32.5]
  :height [1.8])

(deffamily (familychain :sentret :furret))

(deflearnset :gs
  :sentret [:amnesia :attract :curse :cut :defense-curl :detect :dig :double-edge :double-team :dynamicpunch :endure :fire-punch :focus-energy :frustration :fury-cutter :fury-swipes :headbutt :hidden-power :ice-punch :iron-tail :mud-slap :protect :pursuit :quick-attack :rest :return :reversal :rollout :shadow-ball :slam :slash :sleep-talk :snore :sunny-day :surf :swagger :swift :tackle :thief :thunderpunch :toxic]
  :furret [:hyper-beam :scratch :strength])

(deflearnset :rs
  :sentret [:amnesia :assist :attract :body-slam :brick-break :cut :defense-curl :dig :double-edge :double-team :dynamicpunch :endure :facade :fire-punch :flamethrower :focus-energy :focus-punch :follow-me :frustration :fury-cutter :fury-swipes :helping-hand :hidden-power :ice-beam :ice-punch :iron-tail :mimic :mud-slap :protect :pursuit :quick-attack :rain-dance :rest :return :reversal :rollout :scratch :secret-power :shadow-ball :shock-wave :slam :slash :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :swift :thief :thunderbolt :thunderpunch :toxic :trick :water-pulse]
  :furret [:blizzard :hyper-beam :rock-smash :strength :thunder])

(deflearnset :dp
  :sentret [:amnesia :aqua-tail :assist :attract :baton-pass :brick-break :captivate :charge-beam :charm :covet :cut :defense-curl :dig :double-edge :double-team :endure :facade :fire-punch :flamethrower :fling :focus-energy :focus-punch :follow-me :foresight :frustration :fury-cutter :fury-swipes :grass-knot :headbutt :helping-hand :hidden-power :hyper-voice :ice-beam :ice-punch :iron-tail :knock-off :last-resort :me-first :mud-slap :natural-gift :protect :pursuit :quick-attack :rain-dance :rest :return :reversal :rollout :scratch :secret-power :shadow-ball :shadow-claw :shock-wave :slam :slash :sleep-talk :snore :solarbeam :substitute :sucker-punch :sunny-day :super-fang :surf :swagger :swift :thief :thunderbolt :thunderpunch :toxic :trick :u-turn :uproar :water-pulse :whirlpool]
  :furret [:blizzard :focus-blast :giga-impact :hyper-beam :rock-smash :strength :thunder])

(deflearnset :bw
  :sentret [:amnesia :aqua-tail :assist :attract :baton-pass :brick-break :charge-beam :charm :covet :cut :defense-curl :dig :double-edge :double-team :echoed-voice :facade :fire-punch :flamethrower :fling :focus-energy :follow-me :foresight :frustration :fury-swipes :grass-knot :helping-hand :hidden-power :hone-claws :hyper-voice :ice-beam :ice-punch :iron-tail :knock-off :last-resort :me-first :natural-gift :protect :pursuit :quick-attack :rain-dance :rest :retaliate :return :reversal :round :scratch :shadow-ball :shadow-claw :slam :slash :sleep-talk :snore :solarbeam :substitute :sucker-punch :sunny-day :super-fang :surf :swagger :thief :thunderbolt :thunderpunch :toxic :trick :u-turn :uproar :work-up]
  :furret [:blizzard :focus-blast :giga-impact :hyper-beam :rock-smash :strength :thunder])
