
(in-ns 'smogon.dex)


(defpokemon :minun
  :name "Minun"
  :introduced-in :rs
  :types [[:electric]]
  :egggroups [[:fairy]]
  :abilities [[:minus]
              :bw [:minus]]
  :stats [[60 40 50 75 85 95]]
  :weight [4.2]
  :height [0.4])

(deffamily (familychain :minun))

(deflearnset :rs
  :minun [:agility :attract :baton-pass :body-slam :charge :charm :counter :defense-curl :double-edge :double-team :dynamicpunch :encore :endure :facade :flash :frustration :growl :helping-hand :hidden-power :iron-tail :light-screen :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :quick-attack :rain-dance :rest :return :rollout :secret-power :seismic-toss :shock-wave :sleep-talk :snore :spark :substitute :swagger :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :wish :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :minun [:agility :attract :baton-pass :captivate :charge :charge-beam :charm :copycat :discharge :double-team :encore :endure :facade :fake-tears :flash :fling :frustration :grass-knot :growl :headbutt :helping-hand :hidden-power :iron-tail :last-resort :light-screen :magnet-rise :mud-slap :nasty-plot :natural-gift :protect :quick-attack :rain-dance :rest :return :rollout :secret-power :shock-wave :signal-beam :sing :sleep-talk :snore :spark :substitute :swagger :sweet-kiss :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trump-card :uproar :wish :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :minun [:agility :attract :baton-pass :charge :charge-beam :charm :copycat :discharge :double-team :echoed-voice :electro-ball :encore :entrainment :facade :fake-tears :flash :fling :frustration :grass-knot :growl :helping-hand :hidden-power :iron-tail :last-resort :light-screen :lucky-chant :magnet-rise :nasty-plot :protect :quick-attack :rain-dance :rest :return :round :signal-beam :sing :sleep-talk :snore :spark :substitute :swagger :sweet-kiss :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trump-card :uproar :volt-switch :wild-charge :wish :body-slam :double-edge :mimic :seismic-toss])
