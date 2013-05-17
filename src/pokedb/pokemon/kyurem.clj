
(in-ns 'smogon.dex)


(defpokemon :kyurem
  :name "Kyurem"
  :introduced-in :bw
  :types [[:dragon :ice]]
  :egggroups [[]]
  :abilities [[:pressure]]
  :stats [[125 130 90 130 90 95]]
  :weight [325]
  :height [3])

(defpokemon :kyurem-b
  :name "Kyurem-Black"
  :introduced-in :bw
  :types [[:dragon :ice]]
  :egggroups [[]]
  :abilities [[:teravolt]]
  :stats [[125 170 100 120 90 95]]
  :weight [325]
  :height [3.3])

(defpokemon :kyurem-w
  :name "Kyurem-White"
  :introduced-in :bw
  :types [[:dragon :ice]]
  :egggroups [[]]
  :abilities [[:turboblaze]]
  :stats [[125 120 90 170 100 95]]
  :weight [325]
  :height [3.6])

(deffamily (familychain :kyurem [:kyurem-b :kyurem-w]))

(deflearnset :bw
  :kyurem [:ancientpower :blizzard :cut :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-rage :dragon-tail :dragonbreath :earth-power :echoed-voice :endeavor :facade :flash-cannon :fling :fly :focus-blast :frustration :giga-impact :glaciate :hail :hidden-power :hone-claws :hyper-beam :hyper-voice :ice-beam :icy-wind :imprison :iron-head :light-screen :outrage :payback :protect :psychic :rain-dance :reflect :rest :return :rock-slide :rock-smash :rock-tomb :roost :round :safeguard :scary-face :shadow-ball :shadow-claw :signal-beam :slash :sleep-talk :snore :stone-edge :strength :substitute :sunny-day :swagger :toxic :zen-headbutt])

