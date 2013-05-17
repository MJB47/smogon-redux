
(in-ns 'smogon.dex)

(defpokemon :zekrom
  :name "Zekrom"
  :introduced-in :bw
  :types [[:dragon :electric]]
  :egggroups [[]]
  :abilities [[:teravolt]]
  :stats [[100 150 120 120 100 90]]
  :weight [345]
  :height [2.9])

(deffamily (familychain :zekrom))

(deflearnset :bw
  :zekrom [:ancientpower :bolt-strike :charge-beam :crunch :cut :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-rage :dragon-tail :dragonbreath :earth-power :echoed-voice :facade :flash :flash-cannon :fling :fly :focus-blast :frustration :fusion-bolt :giga-impact :haze :hidden-power :hone-claws :hyper-beam :hyper-voice :imprison :light-screen :magnet-rise :outrage :payback :protect :psychic :rain-dance :reflect :rest :return :rock-slide :rock-smash :rock-tomb :roost :round :safeguard :shadow-ball :shadow-claw :signal-beam :slash :sleep-talk :snore :stone-edge :strength :substitute :swagger :tailwind :thunder :thunder-fang :thunder-wave :thunderbolt :thunderpunch :toxic :volt-switch :wild-charge :zen-headbutt])

