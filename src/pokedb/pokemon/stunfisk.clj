
(in-ns 'smogon.dex)


(defpokemon :stunfisk
  :name "Stunfisk"
  :introduced-in :bw
  :types [[:electric :ground]]
  :egggroups [[:indeterminate :water-1]]
  :abilities [[:limber :sand-veil :static]]
  :stats [[109 66 84 81 99 32]]
  :weight [11]
  :height [0.7])

(deffamily (familychain :stunfisk))

(deflearnset :bw
  :stunfisk [:aqua-tail :astonish :attract :bide :bounce :bulldoze :camouflage :curse :dig :discharge :double-team :earth-power :earthquake :electroweb :endeavor :endure :facade :fissure :flail :flash :foul-play :frustration :hidden-power :magnet-rise :mud-bomb :mud-shot :mud-slap :mud-sport :muddy-water :pain-split :payback :protect :rain-dance :rest :return :revenge :rock-slide :rock-tomb :round :sandstorm :scald :shock-wave :sleep-talk :sludge-bomb :sludge-wave :snore :spark :spite :stealth-rock :stone-edge :substitute :surf :swagger :thunder :thunder-wave :thunderbolt :thundershock :toxic :uproar :yawn])

