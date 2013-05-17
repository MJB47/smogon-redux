
(in-ns 'smogon.dex)


(defpokemon :skitty
  :name "Skitty"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:fairy :ground]]
  :abilities [[:cute-charm]
              :dp [:cute-charm :normalize]
              :bw [:cute-charm :normalize :wonder-skin]]
  :stats [[50 45 45 35 35 50]]
  :weight [11]
  :height [0.6])

(defpokemon :delcatty
  :name "Delcatty"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:fairy :ground]]
  :abilities [[:cute-charm]
              :dp [:cute-charm :normalize]
              :bw [:cute-charm :normalize :wonder-skin]]
  :stats [[70 65 65 55 55 70]]
  :weight [32.6]
  :height [1.1])

(deffamily (familychain :skitty :delcatty))

(deflearnset :rs
  :skitty [:assist :attract :baton-pass :blizzard :body-slam :calm-mind :charm :covet :defense-curl :dig :double-edge :double-team :doubleslap :dream-eater :endure :facade :faint-attack :fake-tears :flash :frustration :growl :heal-bell :helping-hand :hidden-power :ice-beam :icy-wind :iron-tail :mimic :mud-slap :protect :psych-up :rain-dance :rest :return :rollout :safeguard :secret-power :shadow-ball :shock-wave :sing :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :tail-whip :thunder :thunder-wave :thunderbolt :tickle :toxic :uproar :water-pulse :wish :body-slam :double-edge :icy-wind :mimic]
  :delcatty [:hyper-beam :rock-smash :strength])

(deflearnset :dp
  :skitty [:assist :attract :baton-pass :blizzard :calm-mind :captivate :charge-beam :charm :copycat :covet :dig :double-edge :double-team :doubleslap :dream-eater :endure :facade :faint-attack :fake-out :fake-tears :flash :foresight :frustration :grass-knot :growl :headbutt :heal-bell :helping-hand :hidden-power :ice-beam :icy-wind :iron-tail :last-resort :mud-bomb :mud-slap :natural-gift :payback :protect :psych-up :rain-dance :rest :return :rollout :safeguard :secret-power :shadow-ball :shock-wave :sing :sleep-talk :snore :solarbeam :substitute :sucker-punch :sunny-day :swagger :swift :tackle :tail-whip :thunder :thunder-wave :thunderbolt :tickle :toxic :uproar :wake-up-slap :water-pulse :wish :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :delcatty [:giga-impact :hyper-beam :rock-smash :strength])

(deflearnset :bw
  :skitty [:assist :attract :baton-pass :blizzard :calm-mind :captivate :charge-beam :charm :copycat :covet :dig :double-edge :double-team :doubleslap :dream-eater :echoed-voice :facade :faint-attack :fake-out :fake-tears :flash :foresight :frustration :grass-knot :growl :heal-bell :helping-hand :hidden-power :hyper-voice :ice-beam :icy-wind :iron-tail :last-resort :mud-bomb :payback :protect :psych-up :rain-dance :rest :retaliate :return :round :safeguard :shadow-ball :simple-beam :sing :sleep-talk :snore :solarbeam :substitute :sucker-punch :sunny-day :swagger :tackle :tail-whip :thunder :thunder-wave :thunderbolt :tickle :toxic :uproar :wake-up-slap :wild-charge :wish :work-up :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :delcatty [:giga-impact :hyper-beam :rock-smash :strength])
