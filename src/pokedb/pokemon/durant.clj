
(in-ns 'smogon.dex)


(defpokemon :durant
  :name "Durant"
  :introduced-in :bw
  :types [[:bug :steel]]
  :egggroups [[:bug]]
  :abilities [[:hustle :swarm :truant]]
  :stats [[58 109 112 48 48 109]]
  :weight [33]
  :height [0.3])

(deffamily (familychain :durant))

(deflearnset :bw
  :durant [:aerial-ace :agility :attract :baton-pass :bite :bug-bite :crunch :cut :dig :double-team :endeavor :endure :energy-ball :entrainment :facade :faint-attack :flash-cannon :frustration :fury-cutter :giga-impact :guillotine :hidden-power :hone-claws :iron-defense :iron-head :metal-claw :metal-sound :protect :rest :retaliate :return :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :round :sand-attack :sandstorm :screech :shadow-claw :sleep-talk :snore :stone-edge :strength :struggle-bug :substitute :superpower :swagger :thunder-fang :thunder-wave :toxic :vicegrip :x-scissor])
