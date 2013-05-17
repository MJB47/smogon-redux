
(in-ns 'smogon.dex)


(defpokemon :latias
  :name "Latias"
  :introduced-in :rs
  :types [[:dragon :psychic]]
  :egggroups [[]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[80 80 90 110 130 110]]
  :weight [40]
  :height [1.4])

(deffamily (familychain :latias))

(deflearnset :rs
  :latias [:aerial-ace :attract :body-slam :calm-mind :charm :cut :dive :double-edge :double-team :dragon-claw :dragonbreath :dream-eater :earthquake :endure :facade :flash :fly :frustration :fury-cutter :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :light-screen :mimic :mist-ball :mud-slap :protect :psych-up :psychic :psywave :rain-dance :recover :reflect :refresh :rest :return :roar :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :water-pulse :water-sport :waterfall :wish :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :latias [:aerial-ace :attract :calm-mind :captivate :charge-beam :charm :cut :defog :dive :double-team :draco-meteor :dragon-claw :dragon-pulse :dragonbreath :dream-eater :earthquake :endure :energy-ball :facade :flash :fly :frustration :fury-cutter :giga-impact :grass-knot :healing-wish :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :last-resort :light-screen :magic-coat :mist-ball :mud-slap :natural-gift :outrage :protect :psych-up :psychic :psycho-shift :psywave :rain-dance :recover :reflect :refresh :rest :return :roar :role-play :roost :safeguard :sandstorm :secret-power :shadow-ball :shadow-claw :shock-wave :sleep-talk :snore :solarbeam :steel-wing :substitute :sucker-punch :sunny-day :surf :swagger :swift :tailwind :thunder :thunder-wave :thunderbolt :toxic :trick :twister :water-pulse :water-sport :waterfall :whirlpool :wish :zen-headbutt :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :latias [:aerial-ace :attract :bulldoze :calm-mind :charge-beam :charm :covet :cut :dive :double-team :draco-meteor :dragon-claw :dragon-pulse :dragonbreath :dream-eater :earthquake :energy-ball :facade :flash :fly :frustration :giga-impact :grass-knot :guard-split :heal-pulse :healing-wish :helping-hand :hidden-power :hone-claws :hyper-beam :ice-beam :icy-wind :last-resort :light-screen :magic-coat :magic-room :mist-ball :outrage :protect :psych-up :psychic :psycho-shift :psyshock :psywave :rain-dance :recover :reflect :reflect-type :refresh :rest :retaliate :return :roar :role-play :roost :round :safeguard :sandstorm :shadow-ball :shadow-claw :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :tailwind :telekinesis :thunder :thunder-wave :thunderbolt :toxic :trick :water-sport :waterfall :wish :zen-headbutt :body-slam :double-edge :icy-wind :mimic])
