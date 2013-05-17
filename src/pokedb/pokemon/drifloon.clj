
(in-ns 'smogon.dex)


(defpokemon :drifloon
  :name "Drifloon"
  :introduced-in :dp
  :types [[:flying :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:aftermath :flare-boost :unburden]]
  :stats [[90 50 34 60 44 70]]
  :weight [1.2]
  :height [0.4])

(defpokemon :drifblim
  :name "Drifblim"
  :introduced-in :dp
  :types [[:flying :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:aftermath :flare-boost :unburden]]
  :stats [[150 80 44 90 54 80]]
  :weight [15]
  :height [1.2])

(deffamily (familychain :drifloon :drifblim))

(deflearnset :dp
  :drifloon [:air-cutter :astonish :attract :baton-pass :body-slam :calm-mind :captivate :charge-beam :constrict :cut :defog :destiny-bond :disable :double-team :dream-eater :embargo :endure :explosion :facade :flash :focus-energy :frustration :gust :gyro-ball :haze :hidden-power :hypnosis :icy-wind :knock-off :magic-coat :memento :minimize :mud-slap :natural-gift :ominous-wind :pain-split :payback :protect :psych-up :psychic :rain-dance :recycle :rest :return :rollout :secret-power :shadow-ball :shock-wave :silver-wind :skill-swap :sleep-talk :snore :spit-up :spite :stockpile :substitute :sucker-punch :sunny-day :swagger :swallow :swift :tailwind :thief :thunder :thunder-wave :thunderbolt :toxic :trick :weather-ball :will-o-wisp]
  :drifblim [:fly :giga-impact :hyper-beam])

(deflearnset :bw
  :drifloon [:acrobatics :amnesia :astonish :attract :baton-pass :bind :body-slam :calm-mind :charge-beam :clear-smog :constrict :cut :defog :destiny-bond :disable :double-team :dream-eater :embargo :explosion :facade :flash :focus-energy :frustration :gust :gyro-ball :haze :hex :hidden-power :hypnosis :icy-wind :knock-off :magic-coat :memento :minimize :ominous-wind :pain-split :payback :protect :psych-up :psychic :rain-dance :recycle :rest :return :round :shadow-ball :skill-swap :sleep-talk :snore :spit-up :spite :stockpile :substitute :sunny-day :swagger :swallow :tailwind :telekinesis :thief :thunder :thunder-wave :thunderbolt :toxic :trick :weather-ball :will-o-wisp]
  :drifblim [:fly :giga-impact :hyper-beam])
