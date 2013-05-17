
(in-ns 'smogon.dex)

(defpokemon :pichu
  :name "Pichu"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[]]
  :abilities [[:static]
              :bw [:lightningrod :static]]
  :stats [[20 40 15 35 35 60]]
  :weight [2]
  :height [0.3])

(defpokemon :pikachu
  :name "Pikachu"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:fairy :ground]]
  :abilities [[:static]
              :bw [:lightningrod :static]]
  :stats [[35 55 30 50 90]
          :gs [35 55 30 50 40 90]]
  :weight [6]
  :height [0.4])

(defpokemon :raichu
  :name "Raichu"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:fairy :ground]]
  :abilities [[:static]
              :bw [:lightningrod :static]]
  :stats [[60 90 55 90 100]
          :gs [60 90 55 90 80 100]]
  :weight [30]
  :height [0.8])

(deffamily (familychain :pichu :pikachu :raichu))

(deflearnset :rb
  :pikachu [:agility :bide :body-slam :double-edge :double-team :flash :growl :light-screen :mega-kick :mega-punch :mimic :pay-day :quick-attack :rage :reflect :rest :seismic-toss :skull-bash :slam :submission :substitute :surf :swift :tail-whip :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :fly :dizzy-punch :petal-dance :sing :scary-face]
  :raichu [:hyper-beam])

(deflearnset :gs
  :pichu [:attract :bide :charm :curse :defense-curl :detect :double-team :doubleslap :encore :endure :flash :frustration :headbutt :hidden-power :iron-tail :mud-slap :present :protect :rain-dance :rest :return :reversal :rollout :sleep-talk :snore :swagger :sweet-kiss :swift :tail-whip :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon :dizzy-punch :petal-dance :sing :scary-face]
  :pikachu [:agility :dynamicpunch :growl :light-screen :quick-attack :slam :strength :thunderpunch :fly :surf]
  :raichu [:hyper-beam :thief])

(deflearnset :rs
  :pichu [:attract :bide :body-slam :charge :charm :counter :defense-curl :double-edge :double-team :doubleslap :encore :endure :facade :flash :frustration :hidden-power :iron-tail :light-screen :mega-kick :mega-punch :mimic :mud-slap :present :protect :rain-dance :rest :return :reversal :rollout :secret-power :seismic-toss :shock-wave :sleep-talk :snore :substitute :swagger :sweet-kiss :swift :tail-whip :thunder :thunder-wave :thunderbolt :thundershock :toxic :volt-tackle :wish :follow-me :teeter-dance :wish]
  :pikachu [:agility :brick-break :dig :dynamicpunch :focus-punch :growl :quick-attack :rock-smash :slam :strength :thunderpunch :fly :body-slam :double-edge :mimic :seismic-toss]
  :raichu [:hyper-beam :thief :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :pichu [:attract :bide :captivate :charge :charge-beam :charm :double-team :doubleslap :encore :endure :facade :fake-out :flail :flash :fling :frustration :grass-knot :headbutt :helping-hand :hidden-power :iron-tail :light-screen :magnet-rise :mud-slap :nasty-plot :natural-gift :present :protect :rain-dance :rest :return :reversal :rollout :secret-power :shock-wave :signal-beam :sleep-talk :snore :substitute :swagger :sweet-kiss :swift :tail-whip :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :tickle :toxic :uproar :volt-tackle :wish :teeter-dance :follow-me]
  :pikachu [:agility :brick-break :dig :discharge :feint :focus-punch :growl :knock-off :quick-attack :rock-smash :slam :strength :surf :last-resort :yawn :fly :body-slam :double-edge :mimic :seismic-toss]
  :raichu [:focus-blast :giga-impact :hyper-beam :thief :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :pichu [:attract :bestow :bide :charge :charge-beam :charm :covet :double-team :doubleslap :echoed-voice :encore :endure :facade :fake-out :flail :flash :fling :frustration :grass-knot :helping-hand :hidden-power :iron-tail :light-screen :lucky-chant :magnet-rise :nasty-plot :present :protect :rain-dance :rest :return :reversal :round :signal-beam :sleep-talk :snore :substitute :swagger :sweet-kiss :tail-whip :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :tickle :toxic :uproar :volt-switch :volt-tackle :wild-charge :wish :teeter-dance :follow-me]
  :pikachu [:agility :brick-break :dig :discharge :electro-ball :feint :growl :knock-off :quick-attack :rock-smash :slam :strength :fly :sing :extremespeed :surf :last-resort :yawn :body-slam :double-edge :mimic :seismic-toss]
  :raichu [:focus-blast :giga-impact :hyper-beam :thief :body-slam :double-edge :mimic :seismic-toss])
