
(in-ns 'smogon.dex)


(defpokemon :poochyena
  :name "Poochyena"
  :introduced-in :rs
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:run-away]
              :dp [:quick-feet :run-away]
              :bw [:quick-feet :rattled :run-away]]
  :stats [[35 55 35 30 30 35]]
  :weight [13.6]
  :height [0.5])

(defpokemon :mightyena
  :name "Mightyena"
  :introduced-in :rs
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:intimidate]
              :dp [:intimidate :quick-feet]
              :bw [:intimidate :moxie :quick-feet]]
  :stats [[70 90 70 60 60 70]]
  :weight [37]
  :height [1])

(deffamily (familychain :poochyena :mightyena))

(deflearnset :rs
  :poochyena [:astonish :attract :bite :body-slam :counter :covet :crunch :dig :double-edge :double-team :endure :facade :frustration :hidden-power :howl :iron-tail :leer :mimic :mud-slap :odor-sleuth :poison-fang :protect :psych-up :rain-dance :rest :return :roar :rock-smash :sand-attack :scary-face :secret-power :shadow-ball :sleep-talk :snatch :snore :substitute :sunny-day :swagger :tackle :take-down :taunt :thief :torment :toxic :yawn :body-slam :double-edge :heal-bell :mimic]
  :mightyena [:hyper-beam :strength :body-slam :double-edge :mimic])

(deflearnset :dp
  :poochyena [:assurance :astonish :attract :bite :captivate :covet :crunch :dark-pulse :dig :double-team :embargo :endure :facade :fire-fang :frustration :headbutt :hidden-power :howl :ice-fang :iron-tail :leer :me-first :mud-slap :natural-gift :odor-sleuth :payback :poison-fang :protect :rain-dance :rest :return :roar :rock-smash :sand-attack :scary-face :secret-power :shadow-ball :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :super-fang :swagger :tackle :take-down :taunt :thief :thunder-fang :torment :toxic :uproar :yawn :body-slam :double-edge :heal-bell :mimic]
  :mightyena [:giga-impact :hyper-beam :strength :body-slam :double-edge :mimic])

(deflearnset :bw
  :poochyena [:assurance :astonish :attract :bite :covet :crunch :dark-pulse :dig :double-team :embargo :facade :fire-fang :foul-play :frustration :hidden-power :howl :hyper-voice :ice-fang :incinerate :iron-tail :leer :me-first :odor-sleuth :payback :poison-fang :protect :rain-dance :rest :retaliate :return :roar :rock-smash :round :sand-attack :scary-face :shadow-ball :sleep-talk :snarl :snatch :snore :spite :substitute :sucker-punch :sunny-day :super-fang :swagger :tackle :take-down :taunt :thief :thunder-fang :torment :toxic :uproar :yawn :body-slam :double-edge :heal-bell :mimic]
  :mightyena [:giga-impact :hyper-beam :strength :body-slam :double-edge :mimic])
