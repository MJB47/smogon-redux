
(in-ns 'smogon.dex)


(defpokemon :swablu
  :name "Swablu"
  :introduced-in :rs
  :types [[:flying :normal]]
  :egggroups [[:dragon :flying]]
  :abilities [[:natural-cure]
              :bw [:cloud-nine :natural-cure]]
  :stats [[45 40 60 40 75 50]]
  :weight [1.2]
  :height [0.4])

(defpokemon :altaria
  :name "Altaria"
  :introduced-in :rs
  :types [[:dragon :flying]]
  :egggroups [[:dragon :flying]]
  :abilities [[:natural-cure]
              :bw [:cloud-nine :natural-cure]]
  :stats [[75 70 90 70 105 80]]
  :weight [20.6]
  :height [1.1])

(deffamily (familychain :swablu :altaria))



(deflearnset :rs
  :swablu [:aerial-ace :agility :astonish :attract :body-slam :double-edge :double-team :dream-eater :endure :facade :fly :frustration :fury-attack :growl :haze :hidden-power :ice-beam :mimic :mirror-move :mist :mud-slap :peck :perish-song :protect :psych-up :pursuit :rage :rain-dance :refresh :rest :return :safeguard :secret-power :sing :sky-attack :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :swagger :swift :take-down :thief :toxic :body-slam :double-edge :mimic :sky-attack]
  :altaria [:dragon-claw :dragon-dance :dragonbreath :earthquake :fire-blast :flamethrower :hyper-beam :iron-tail :roar :rock-smash])

(deflearnset :dp
  :swablu [:aerial-ace :agility :air-cutter :astonish :attract :captivate :double-team :dragon-pulse :dragon-rush :dream-eater :endure :facade :featherdance :fly :frustration :fury-attack :growl :haze :heal-bell :heat-wave :hidden-power :ice-beam :mirror-move :mist :mud-slap :natural-gift :ominous-wind :outrage :peck :perish-song :pluck :power-swap :protect :psych-up :pursuit :rage :rain-dance :refresh :rest :return :roost :safeguard :secret-power :sing :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :swagger :swift :tailwind :take-down :thief :toxic :twister :uproar :body-slam :double-edge :mimic :sky-attack]
  :altaria [:draco-meteor :dragon-claw :dragon-dance :dragonbreath :earthquake :fire-blast :flamethrower :giga-impact :hyper-beam :iron-tail :roar :rock-smash :sky-attack])

(deflearnset :bw
  :swablu [:aerial-ace :agility :astonish :attract :cotton-guard :double-team :dragon-pulse :dragon-rush :dream-eater :echoed-voice :facade :featherdance :fly :frustration :fury-attack :growl :haze :heal-bell :heat-wave :hidden-power :hyper-voice :ice-beam :mirror-move :mist :natural-gift :outrage :peck :perish-song :pluck :power-swap :protect :psych-up :pursuit :rage :rain-dance :refresh :rest :return :roost :round :safeguard :sing :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :swagger :tailwind :take-down :thief :toxic :uproar :body-slam :double-edge :mimic :sky-attack]
  :altaria [:bulldoze :draco-meteor :dragon-claw :dragon-dance :dragonbreath :earthquake :fire-blast :flamethrower :giga-impact :hone-claws :hyper-beam :incinerate :iron-tail :roar :rock-smash :sky-attack :wonder-room])

