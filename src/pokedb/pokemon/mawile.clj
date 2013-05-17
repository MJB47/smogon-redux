
(in-ns 'smogon.dex)


(defpokemon :mawile
  :name "Mawile"
  :introduced-in :rs
  :types [[:steel]]
  :egggroups [[:fairy :ground]]
  :abilities [[:hyper-cutter :intimidate]
              :bw [:hyper-cutter :intimidate :sheer-force]]
  :stats [[50 85 85 55 55 50]]
  :weight [11.5]
  :height [0.6])

(deffamily (familychain :mawile))

(deflearnset :rs
  :mawile [:ancientpower :astonish :attract :baton-pass :bite :body-slam :brick-break :counter :crunch :double-edge :double-team :dynamicpunch :endure :facade :faint-attack :fake-tears :false-swipe :fire-blast :flamethrower :focus-punch :frustration :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-defense :mega-kick :mega-punch :mimic :mud-slap :poison-fang :protect :psych-up :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :sandstorm :secret-power :seismic-toss :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :sweet-scent :swords-dance :taunt :thunderpunch :tickle :torment :toxic :vicegrip :body-slam :double-edge :icy-wind :mimic :seismic-toss :sing])

(deflearnset :dp
  :mawile [:ancientpower :astonish :attract :baton-pass :bite :brick-break :captivate :charge-beam :crunch :dark-pulse :double-team :embargo :endure :facade :faint-attack :fake-tears :false-swipe :fire-blast :fire-fang :flamethrower :flash-cannon :fling :focus-blast :focus-punch :frustration :giga-impact :grass-knot :guard-swap :headbutt :hidden-power :hyper-beam :ice-beam :ice-fang :ice-punch :icy-wind :iron-defense :iron-head :knock-off :magnet-rise :mud-slap :natural-gift :pain-split :payback :poison-fang :protect :psych-up :punishment :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :sandstorm :secret-power :shadow-ball :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stockpile :strength :substitute :sucker-punch :sunny-day :super-fang :swagger :swallow :sweet-scent :swords-dance :taunt :thunder-fang :thunderpunch :tickle :torment :toxic :vicegrip :body-slam :double-edge :icy-wind :mimic :seismic-toss :sing])

(deflearnset :bw
  :mawile [:ancientpower :astonish :attract :baton-pass :bite :brick-break :captivate :charge-beam :crunch :dark-pulse :double-team :embargo :facade :faint-attack :fake-tears :false-swipe :fire-blast :fire-fang :flamethrower :flash-cannon :fling :focus-blast :foul-play :frustration :giga-impact :grass-knot :guard-swap :hidden-power :hyper-beam :ice-beam :ice-fang :ice-punch :icy-wind :incinerate :iron-defense :iron-head :knock-off :last-resort :magnet-rise :metal-burst :pain-split :payback :poison-fang :protect :psych-up :punishment :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :round :sandstorm :shadow-ball :slam :sleep-talk :sludge-bomb :snatch :snore :solarbeam :spit-up :stealth-rock :stockpile :stone-edge :strength :substitute :sucker-punch :sunny-day :super-fang :swagger :swallow :sweet-scent :swords-dance :taunt :thunder-fang :thunderpunch :tickle :torment :toxic :vicegrip :body-slam :double-edge :icy-wind :mimic :seismic-toss :sing])
