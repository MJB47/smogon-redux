
(in-ns 'smogon.dex)


(defpokemon :plusle
  :name "Plusle"
  :introduced-in :rs
  :types [[:electric]]
  :egggroups [[:fairy]]
  :abilities [[:plus]
              :bw [:plus]]
  :stats [[60 50 40 85 75 95]]
  :weight [4.2]
  :height [0.4])

(deffamily (familychain :plusle))

(deflearnset :rs
  :plusle [:agility :attract :baton-pass :body-slam :charge :counter :defense-curl :double-edge :double-team :dynamicpunch :encore :endure :facade :fake-tears :flash :frustration :growl :helping-hand :hidden-power :iron-tail :light-screen :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :quick-attack :rain-dance :rest :return :rollout :secret-power :seismic-toss :shock-wave :sleep-talk :snore :spark :substitute :swagger :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :wish :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :plusle [:agility :attract :baton-pass :captivate :charge :charge-beam :copycat :discharge :double-team :encore :endure :facade :fake-tears :flash :fling :frustration :grass-knot :growl :headbutt :helping-hand :hidden-power :iron-tail :last-resort :light-screen :magnet-rise :mud-slap :nasty-plot :natural-gift :protect :quick-attack :rain-dance :rest :return :rollout :secret-power :shock-wave :signal-beam :sing :sleep-talk :snore :spark :substitute :swagger :sweet-kiss :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :wish :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :plusle [:agility :attract :baton-pass :charge :charge-beam :copycat :discharge :double-team :echoed-voice :electro-ball :encore :entrainment :facade :fake-tears :flash :fling :frustration :grass-knot :growl :helping-hand :hidden-power :iron-tail :last-resort :light-screen :lucky-chant :magnet-rise :nasty-plot :protect :quick-attack :rain-dance :rest :return :round :signal-beam :sing :sleep-talk :snore :spark :substitute :swagger :sweet-kiss :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :volt-switch :wild-charge :wish :body-slam :double-edge :mimic :seismic-toss])
