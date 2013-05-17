
(in-ns 'smogon.dex)


(defpokemon :giratina
  :name "Giratina"
  :introduced-in :dp
  :types [[:dragon :ghost]]
  :egggroups [[]]
  :abilities [[:pressure :telepathy]]
  :stats [[150 100 120 100 120 90]]
  :weight [750]
  :height [4.5])

(defpokemon :giratina-o
  :name "Giratina-Origin"
  :introduced-in :dp
  :types [[:dragon :ghost]]
  :egggroups [[]]
  :abilities [[:levitate]]
  :stats [[150 120 100 120 100 90]]
  :weight [650]
  :height [6.9])

(deffamily
  (familyclique :giratina :giratina-o))

(deflearnset :dp
  :giratina [:aerial-ace :air-cutter :ancientpower :aqua-tail :aura-sphere :calm-mind :charge-beam :cut :dark-pulse :defog :destiny-bond :double-team :draco-meteor :dragon-claw :dragon-pulse :dragonbreath :dream-eater :earth-power :earthquake :endure :energy-ball :facade :fly :frustration :fury-cutter :giga-impact :gravity :headbutt :heal-block :hidden-power :hyper-beam :icy-wind :iron-head :iron-tail :mud-slap :natural-gift :ominous-wind :outrage :pain-split :payback :protect :psych-up :psychic :rain-dance :rest :return :roar :rock-climb :rock-smash :safeguard :scary-face :secret-power :shadow-ball :shadow-claw :shadow-force :shadow-sneak :shock-wave :silver-wind :slash :sleep-talk :snore :spite :steel-wing :stone-edge :strength :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :twister :will-o-wisp])

(deflearnset :bw
  :giratina [:aerial-ace :ancientpower :aqua-tail :aura-sphere :bulldoze :calm-mind :charge-beam :cut :dark-pulse :destiny-bond :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-tail :dragonbreath :dream-eater :earth-power :earthquake :echoed-voice :energy-ball :facade :fly :frustration :giga-impact :gravity :hex :hidden-power :hone-claws :hyper-beam :hyper-voice :icy-wind :iron-head :iron-tail :ominous-wind :outrage :pain-split :payback :protect :psych-up :psychic :rain-dance :rest :return :roar :rock-smash :round :safeguard :scary-face :shadow-ball :shadow-claw :shadow-force :shadow-sneak :slash :sleep-talk :snore :spite :stone-edge :strength :substitute :sunny-day :swagger :telekinesis :thunder :thunder-wave :thunderbolt :toxic :will-o-wisp])
