
(in-ns 'smogon.dex)


(defpokemon :sigilyph
  :name "Sigilyph"
  :introduced-in :bw
  :types [[:flying :psychic]]
  :egggroups [[:flying]]
  :abilities [[:magic-guard :tinted-lens :wonder-skin]]
  :stats [[72 58 80 103 80 97]]
  :weight [14]
  :height [1.4])

(deffamily (familychain :sigilyph))

(deflearnset :bw
  :sigilyph [:aerial-ace :air-cutter :air-slash :ancientpower :attract :calm-mind :charge-beam :cosmic-power :dark-pulse :double-team :dream-eater :energy-ball :facade :flash :flash-cannon :fly :frustration :gravity :gust :heat-wave :hidden-power :hyper-beam :hypnosis :ice-beam :icy-wind :light-screen :magic-coat :magic-room :miracle-eye :mirror-move :pluck :protect :psybeam :psych-up :psychic :psycho-shift :psyshock :psywave :rain-dance :reflect :rest :return :roost :round :safeguard :shadow-ball :signal-beam :skill-swap :sky-attack :sleep-talk :smack-down :snore :solarbeam :steel-wing :stored-power :substitute :swagger :synchronoise :tailwind :telekinesis :thief :thunder-wave :toxic :trick :trick-room :whirlwind :zen-headbutt])

