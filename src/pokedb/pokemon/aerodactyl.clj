
(in-ns 'smogon.dex)


(defpokemon :aerodactyl
  :name "Aerodactyl"
  :introduced-in :rb
  :types [[:flying :rock]]
  :egggroups [[:flying]]
  :abilities [[:pressure :rock-head]
              :bw [:pressure :rock-head :unnerve]]
  :stats [[80 105 65 60 130]
          :gs [80 105 65 60 75 130]]
  :weight [59]
  :height [1.8])

(deffamily (familychain (familychain :aerodactyl)))

(deflearnset :rb
  :aerodactyl [:agility :bide :bite :double-edge :double-team :dragon-rage :fire-blast :fly :hyper-beam :mimic :rage :razor-wind :reflect :rest :sky-attack :substitute :supersonic :swift :take-down :toxic :whirlwind :wing-attack])

(deflearnset :gs
  :aerodactyl [:agility :ancientpower :attract :bite :curse :detect :double-team :dragonbreath :earthquake :endure :fire-blast :flamethrower :fly :foresight :frustration :headbutt :hidden-power :hyper-beam :iron-tail :protect :pursuit :rain-dance :rest :return :roar :rock-smash :sandstorm :scary-face :sleep-talk :snore :steel-wing :supersonic :swagger :swift :take-down :toxic :whirlwind :wing-attack])

(deflearnset :rs
  :aerodactyl [:aerial-ace :agility :ancientpower :attract :bite :curse :double-edge :double-team :dragon-claw :dragonbreath :earthquake :endure :facade :fire-blast :flamethrower :fly :foresight :frustration :hidden-power :hyper-beam :iron-tail :mimic :protect :pursuit :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :sandstorm :scary-face :secret-power :sky-attack :sleep-talk :snore :steel-wing :strength :substitute :sunny-day :supersonic :swagger :swift :take-down :taunt :thief :torment :toxic :whirlwind :wing-attack])

(deflearnset :dp
  :aerodactyl [:aerial-ace :agility :air-cutter :ancientpower :aqua-tail :assurance :attract :bite :captivate :crunch :curse :defog :double-team :dragon-claw :dragon-pulse :dragonbreath :earth-power :earthquake :endure :facade :fire-blast :fire-fang :flamethrower :fly :foresight :frustration :giga-impact :headbutt :heat-wave :hidden-power :hyper-beam :ice-fang :iron-head :iron-tail :natural-gift :ominous-wind :payback :protect :pursuit :rain-dance :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :roost :sandstorm :scary-face :secret-power :sky-attack :sleep-talk :snore :stealth-rock :steel-wing :stone-edge :strength :substitute :sunny-day :supersonic :swagger :swift :tailwind :take-down :taunt :thief :thunder-fang :torment :toxic :twister :whirlwind :wing-attack])

(deflearnset :bw
  :aerodactyl [:aerial-ace :agility :ancientpower :aqua-tail :assurance :attract :bite :bulldoze :crunch :curse :double-team :dragon-claw :dragon-pulse :dragonbreath :earth-power :earthquake :facade :fire-blast :fire-fang :flamethrower :fly :foresight :frustration :giga-impact :heat-wave :hidden-power :hone-claws :hyper-beam :ice-fang :incinerate :iron-head :iron-tail :payback :protect :pursuit :rain-dance :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :roost :round :sandstorm :scary-face :sky-attack :sky-drop :sleep-talk :smack-down :snore :stealth-rock :steel-wing :stone-edge :strength :substitute :sunny-day :supersonic :swagger :tailwind :take-down :taunt :thief :thunder-fang :torment :toxic :whirlwind :wing-attack])

