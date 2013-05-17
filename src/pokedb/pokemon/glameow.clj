
(in-ns 'smogon.dex)


(defpokemon :glameow
  :name "Glameow"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:keen-eye :limber :own-tempo]]
  :stats [[49 55 42 42 37 85]]
  :weight [3.9]
  :height [0.5])

(defpokemon :purugly
  :name "Purugly"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:defiant :own-tempo :thick-fat]]
  :stats [[71 82 64 64 59 112]]
  :weight [43.8]
  :height [1])

(deffamily (familychain :glameow :purugly))

(deflearnset :dp
  :glameow [:aerial-ace :assist :assurance :attract :bite :captivate :charm :cut :dig :double-team :dream-eater :endure :facade :faint-attack :fake-out :fake-tears :flail :flash :frustration :fury-cutter :fury-swipes :growl :headbutt :hidden-power :hypnosis :iron-tail :knock-off :last-resort :mud-slap :natural-gift :payback :protect :psych-up :quick-attack :rain-dance :rest :return :sand-attack :scratch :secret-power :shadow-ball :shadow-claw :shock-wave :slash :sleep-talk :snatch :snore :substitute :sucker-punch :sunny-day :super-fang :swagger :swift :tail-whip :taunt :thief :thunder :thunderbolt :torment :toxic :u-turn :water-pulse]
  :purugly [:body-slam :giga-impact :hyper-beam :roar :rollout])

(deflearnset :bw
  :glameow [:aerial-ace :assist :assurance :attract :bite :captivate :charm :covet :cut :dig :double-team :dream-eater :echoed-voice :facade :faint-attack :fake-out :fake-tears :flail :flash :foul-play :frustration :fury-swipes :growl :hidden-power :hone-claws :hyper-voice :hypnosis :iron-tail :knock-off :last-resort :payback :protect :psych-up :quick-attack :rain-dance :rest :retaliate :return :round :sand-attack :scratch :shadow-ball :shadow-claw :slash :sleep-talk :snatch :snore :substitute :sucker-punch :sunny-day :super-fang :swagger :tail-whip :taunt :thief :thunder :thunderbolt :torment :toxic :u-turn :wake-up-slap :work-up]
  :purugly [:body-slam :bulldoze :giga-impact :hyper-beam :roar])


