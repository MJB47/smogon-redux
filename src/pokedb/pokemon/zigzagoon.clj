
(in-ns 'smogon.dex)


(defpokemon :zigzagoon
  :name "Zigzagoon"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup]
              :bw [:gluttony :pickup :quick-feet]]
  :stats [[38 30 41 30 41 60]]
  :weight [17.5]
  :height [0.4])

(defpokemon :linoone
  :name "Linoone"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup]
              :dp [:gluttony :pickup]
              :bw [:gluttony :pickup :quick-feet]]
  :stats [[78 70 61 50 61 100]]
  :weight [32.5]
  :height [0.5])

(deffamily (familychain :zigzagoon :linoone))



(deflearnset :rs
  :zigzagoon [:attract :belly-drum :blizzard :body-slam :charm :covet :cut :defense-curl :dig :double-edge :double-team :endure :facade :flail :frustration :fury-cutter :growl :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :mimic :mud-slap :mud-sport :odor-sleuth :pin-missile :protect :pursuit :rain-dance :rest :return :rock-smash :rollout :sand-attack :secret-power :shadow-ball :shock-wave :sleep-talk :snore :substitute :sunny-day :surf :swagger :swift :tackle :tail-whip :thief :thunder :thunder-wave :thunderbolt :tickle :toxic :trick :water-pulse :body-slam :double-edge :icy-wind :mimic]
  :linoone [:fury-swipes :hyper-beam :roar :slash :strength :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :zigzagoon [:attract :belly-drum :blizzard :captivate :charge-beam :charm :covet :cut :dig :double-team :endure :facade :flail :fling :frustration :fury-cutter :grass-knot :growl :gunk-shot :headbutt :helping-hand :hidden-power :ice-beam :icy-wind :iron-tail :last-resort :mud-slap :mud-sport :natural-gift :odor-sleuth :pin-missile :protect :pursuit :rain-dance :rest :return :rock-smash :rollout :sand-attack :secret-power :seed-bomb :shadow-ball :shock-wave :sleep-talk :snore :substitute :sunny-day :super-fang :surf :swagger :swift :tackle :tail-whip :thief :thunder :thunder-wave :thunderbolt :tickle :toxic :trick :water-pulse :whirlpool :body-slam :double-edge :icy-wind :mimic]
  :linoone [:fury-swipes :giga-impact :hyper-beam :roar :shadow-claw :slash :strength :switcheroo :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :zigzagoon [:attract :belly-drum :bestow :blizzard :charge-beam :charm :covet :cut :dig :double-team :echoed-voice :facade :flail :fling :frustration :grass-knot :growl :gunk-shot :headbutt :helping-hand :hidden-power :hone-claws :hyper-voice :ice-beam :icy-wind :iron-tail :last-resort :mud-slap :mud-sport :odor-sleuth :pin-missile :protect :pursuit :rain-dance :rest :retaliate :return :rock-climb :rock-smash :round :sand-attack :seed-bomb :shadow-ball :simple-beam :sleep-talk :snore :substitute :sunny-day :super-fang :surf :swagger :tackle :tail-whip :thief :thunder :thunder-wave :thunderbolt :tickle :toxic :trick :work-up :body-slam :double-edge :icy-wind :mimic]
  :linoone [:fury-swipes :giga-impact :hyper-beam :roar :shadow-claw :slash :strength :switcheroo :body-slam :double-edge :icy-wind :mimic])

