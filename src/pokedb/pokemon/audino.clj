
(in-ns 'smogon.dex)


(defpokemon :audino
  :name "Audino"
  :introduced-in :bw
  :types [[:normal]]
  :egggroups [[:fairy]]
  :abilities [[:healer :klutz :regenerator]]
  :stats [[103 60 86 60 86 50]]
  :weight [31]
  :height [1.1])

(deffamily (familychain :audino))

(deflearnset :bw
  :audino [:after-you :amnesia :attract :bestow :blizzard :calm-mind :charge-beam :covet :dig :double-edge :double-team :doubleslap :drain-punch :dream-eater :echoed-voice :encore :entrainment :facade :fire-blast :fire-punch :flamethrower :flash :fling :frustration :grass-knot :gravity :growl :heal-bell :heal-pulse :healing-wish :helping-hand :hidden-power :hyper-beam :hyper-voice :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :knock-off :last-resort :light-screen :low-kick :lucky-chant :magic-coat :pain-split :pound :protect :psych-up :psychic :psyshock :rain-dance :reflect :refresh :rest :retaliate :return :role-play :round :safeguard :secret-power :shadow-ball :signal-beam :simple-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :substitute :sunny-day :surf :swagger :sweet-kiss :take-down :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick-room :uproar :wild-charge :wish :work-up :yawn :zen-headbutt])
