
(in-ns 'smogon.dex)


(defpokemon :pachirisu
  :name "Pachirisu"
  :introduced-in :dp
  :types [[:electric]]
  :egggroups [[:fairy :ground]]
  :abilities [[:pickup :run-away :volt-absorb]]
  :stats [[60 45 70 45 90 95]]
  :weight [3.9]
  :height [0.4])

(deffamily (familychain :pachirisu))

(deflearnset :dp
  :pachirisu [:attract :bide :bite :captivate :charge-beam :charm :covet :cut :defense-curl :dig :discharge :double-team :endure :facade :fake-tears :flail :flash :flatter :fling :frustration :grass-knot :growl :gunk-shot :headbutt :helping-hand :hidden-power :iron-tail :last-resort :light-screen :magnet-rise :mud-slap :natural-gift :protect :quick-attack :rain-dance :rest :return :rollout :secret-power :seed-bomb :shock-wave :sleep-talk :snore :spark :substitute :super-fang :swagger :sweet-kiss :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :u-turn :uproar])

(deflearnset :bw
  :pachirisu [:attract :bestow :bide :bite :charge :charge-beam :charm :covet :cut :defense-curl :dig :discharge :double-team :echoed-voice :electro-ball :endure :facade :fake-tears :flail :flash :flatter :fling :follow-me :frustration :grass-knot :growl :gunk-shot :helping-hand :hidden-power :hyper-fang :iron-tail :last-resort :light-screen :magnet-rise :protect :quick-attack :rain-dance :rest :return :rollout :round :seed-bomb :sleep-talk :snore :spark :substitute :super-fang :swagger :sweet-kiss :swift :tail-whip :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :u-turn :uproar :volt-switch])
