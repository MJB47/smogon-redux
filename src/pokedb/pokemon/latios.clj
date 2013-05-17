
(in-ns 'smogon.dex)


(defpokemon :latios
  :name "Latios"
  :introduced-in :rs
  :types [[:dragon :psychic]]
  :egggroups [[]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[80 90 80 130 110 110]]
  :weight [60]
  :height [2])

(deffamily (familychain :latios))

(deflearnset :rs
  :latios [:aerial-ace :attract :body-slam :calm-mind :cut :dive :double-edge :double-team :dragon-claw :dragon-dance :dragonbreath :dream-eater :earthquake :endure :facade :flash :fly :frustration :fury-cutter :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :light-screen :luster-purge :memento :mimic :mud-slap :protect :psych-up :psychic :psywave :rain-dance :recover :reflect :refresh :rest :return :roar :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :water-pulse :waterfall :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :latios [:aerial-ace :attract :calm-mind :captivate :charge-beam :cut :defog :dive :double-team :draco-meteor :dragon-claw :dragon-dance :dragon-pulse :dragonbreath :dream-eater :earthquake :endure :energy-ball :facade :flash :fly :frustration :fury-cutter :giga-impact :grass-knot :heal-block :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :last-resort :light-screen :luster-purge :magic-coat :memento :mud-slap :natural-gift :outrage :protect :psych-up :psychic :psycho-shift :psywave :rain-dance :recover :reflect :refresh :rest :return :roar :roost :safeguard :sandstorm :secret-power :shadow-ball :shadow-claw :shock-wave :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :surf :swagger :swift :tailwind :thunder :thunder-wave :thunderbolt :toxic :trick :twister :water-pulse :waterfall :whirlpool :zen-headbutt :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :latios [:aerial-ace :attract :bulldoze :calm-mind :charge-beam :cut :dive :double-team :draco-meteor :dragon-claw :dragon-dance :dragon-pulse :dragonbreath :dream-eater :earthquake :energy-ball :facade :flash :fly :frustration :giga-impact :grass-knot :heal-block :heal-pulse :helping-hand :hidden-power :hone-claws :hyper-beam :ice-beam :icy-wind :last-resort :light-screen :luster-purge :magic-coat :memento :outrage :power-split :protect :psych-up :psychic :psycho-shift :psyshock :psywave :rain-dance :recover :reflect :refresh :rest :retaliate :return :roar :roost :round :safeguard :sandstorm :shadow-ball :shadow-claw :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :tailwind :telekinesis :thunder :thunder-wave :thunderbolt :toxic :trick :waterfall :wonder-room :zen-headbutt :body-slam :double-edge :icy-wind :mimic])
