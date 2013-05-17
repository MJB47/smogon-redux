
(in-ns 'smogon.dex)


(defpokemon :spiritomb
  :name "Spiritomb"
  :introduced-in :dp
  :types [[:dark :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:infiltrator :pressure]]
  :stats [[50 92 108 92 108 35]]
  :weight [108]
  :height [1])

(deffamily (familychain :spiritomb))

(deflearnset :dp
  :spiritomb [:attract :calm-mind :captivate :confuse-ray :curse :dark-pulse :destiny-bond :double-team :dream-eater :embargo :endure :facade :faint-attack :flash :frustration :giga-impact :grudge :hidden-power :hyper-beam :hypnosis :icy-wind :imprison :memento :nasty-plot :natural-gift :ominous-wind :pain-split :protect :psych-up :psychic :pursuit :rain-dance :rest :return :rock-tomb :secret-power :shadow-ball :shadow-sneak :shock-wave :silver-wind :sleep-talk :smokescreen :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :torment :toxic :trick :uproar :water-pulse :will-o-wisp])

(deflearnset :bw
  :spiritomb [:attract :calm-mind :captivate :confuse-ray :curse :dark-pulse :destiny-bond :double-team :dream-eater :embargo :facade :faint-attack :flash :foul-play :frustration :giga-impact :grudge :hidden-power :hyper-beam :hypnosis :icy-wind :imprison :memento :nasty-plot :nightmare :ominous-wind :pain-split :protect :psych-up :psychic :pursuit :quash :rain-dance :rest :retaliate :return :rock-tomb :round :shadow-ball :shadow-sneak :sleep-talk :smokescreen :snarl :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :telekinesis :thief :torment :toxic :trick :uproar :will-o-wisp :wonder-room])
