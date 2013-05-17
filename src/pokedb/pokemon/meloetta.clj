
(in-ns 'smogon.dex)


(defpokemon :meloetta
  :name "Meloetta"
  :introduced-in :bw
  :types [[:normal :psychic]]
  :egggroups [[]]
  :abilities [[:serene-grace]]
  :stats [[100 77 77 128 128 90]]
  :weight [6.5]
  :height [0.6])

(deffamily (familychain :meloetta))

(deflearnset :bw
  :meloetta [:acrobatics :brick-break :calm-mind :charge-beam :close-combat :confusion :covet :double-team :drain-punch :dream-eater :dual-chop :echoed-voice :embargo :energy-ball :facade :fire-punch :flash :fling :focus-blast :frustration :giga-impact :grass-knot :gravity :heal-bell :helping-hand :hidden-power :hone-claws :hyper-beam :hyper-voice :ice-punch :knock-off :last-resort :light-screen :low-kick :low-sweep :magic-coat :magic-room :payback :perish-song :protect :psybeam :psych-up :psychic :psyshock :quick-attack :rain-dance :recycle :relic-song :rest :retaliate :return :rock-smash :role-play :round :safeguard :shadow-ball :shadow-claw :signal-beam :sing :skill-swap :sleep-talk :snatch :snore :stone-edge :strength :substitute :sunny-day :swagger :teeter-dance :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :uproar :wake-up-slap :wonder-room :work-up :zen-headbutt])
