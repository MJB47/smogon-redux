
(in-ns 'smogon.dex)


(defpokemon :deino
  :name "Deino"
  :introduced-in :bw
  :types [[:dark :dragon]]
  :egggroups [[:dragon]]
  :abilities [[:hustle]]
  :stats [[52 65 50 45 50 38]]
  :weight [17.3]
  :height [0.8])

(defpokemon :zweilous
  :name "Zweilous"
  :introduced-in :bw
  :types [[:dark :dragon]]
  :egggroups [[:dragon]]
  :abilities [[:hustle]]
  :stats [[72 85 70 65 70 58]]
  :weight [50]
  :height [1.4])

(defpokemon :hydreigon
  :name "Hydreigon"
  :introduced-in :bw
  :types [[:dark :dragon]]
  :egggroups [[:dragon]]
  :abilities [[:levitate]]
  :stats [[92 105 90 125 90 98]]
  :weight [160]
  :height [1.8])

(deffamily (familychain :deino :zweilous :hydreigon))

(deflearnset :bw
  :deino [:aqua-tail :assurance :astonish :attract :bite :body-slam :crunch :dark-pulse :double-hit :double-team :draco-meteor :dragon-pulse :dragon-rage :dragon-rush :dragon-tail :dragonbreath :earth-power :facade :fire-fang :focus-energy :frustration :head-smash :headbutt :hidden-power :hyper-voice :ice-fang :incinerate :outrage :protect :psych-up :rain-dance :rest :return :roar :rock-smash :round :scary-face :screech :slam :sleep-talk :snore :spite :strength :substitute :sunny-day :superpower :swagger :tackle :taunt :thief :thunder-fang :thunder-wave :torment :toxic :uproar :work-up :zen-headbutt]
  :hydreigon [:acrobatics :bulldoze :charge-beam :earthquake :echoed-voice :fire-blast :flamethrower :flash-cannon :fly :focus-blast :giga-impact :heat-wave :hyper-beam :iron-tail :payback :reflect :rock-slide :rock-tomb :roost :signal-beam :stone-edge :surf :tailwind :tri-attack :u-turn])
