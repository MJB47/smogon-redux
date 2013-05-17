
(in-ns 'smogon.dex)


(defpokemon :togepi
  :name "Togepi"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:hustle :serene-grace]
              :bw [:hustle :serene-grace :super-luck]]
  :stats [[35 20 65 40 65 20]]
  :weight [1.5]
  :height [0.3])

(defpokemon :togetic
  :name "Togetic"
  :introduced-in :gs
  :types [[:flying :normal]]
  :egggroups [[:fairy :flying]]
  :abilities [[:hustle :serene-grace]
              :bw [:hustle :serene-grace :super-luck]]
  :stats [[55 40 85 80 105 40]]
  :weight [3.2]
  :height [0.6])

(defpokemon :togekiss
  :name "Togekiss"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:fairy :flying]]
  :abilities [[:hustle :serene-grace :super-luck]]
  :stats [[85 50 95 120 115 80]]
  :weight [38]
  :height [1.5])

(deffamily (familychain :togepi :togetic :togekiss))



(deflearnset :gs
  :togepi [:attract :charm :curse :defense-curl :detect :double-edge :double-team :dream-eater :encore :endure :fire-blast :flamethrower :flash :foresight :frustration :future-sight :growl :headbutt :hidden-power :metronome :mirror-move :mud-slap :peck :present :protect :psych-up :psychic :rain-dance :rest :return :rock-smash :rollout :safeguard :shadow-ball :sleep-talk :snore :solarbeam :sunny-day :swagger :sweet-kiss :swift :toxic :zap-cannon]
  :togetic [:fly :hyper-beam :steel-wing])

(deflearnset :rs
  :togepi [:ancientpower :attract :baton-pass :body-slam :charm :counter :defense-curl :double-edge :double-team :dream-eater :encore :endure :facade :fire-blast :flamethrower :flash :follow-me :foresight :frustration :future-sight :growl :hidden-power :light-screen :mega-kick :mega-punch :metronome :mimic :mirror-move :mud-slap :peck :present :protect :psych-up :psychic :rain-dance :reflect :rest :return :rock-smash :rollout :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :snore :softboiled :solarbeam :substitute :sunny-day :swagger :sweet-kiss :swift :thunder-wave :toxic :water-pulse :wish :yawn :helping-hand :tri-attack]
  :togetic [:aerial-ace :brick-break :fly :focus-punch :hyper-beam :magical-leaf :sky-attack :steel-wing])

(deflearnset :dp
  :togepi [:ancientpower :attract :baton-pass :captivate :charm :double-edge :double-team :dream-eater :encore :endeavor :endure :extrasensory :facade :fire-blast :flamethrower :flash :fling :follow-me :foresight :frustration :future-sight :grass-knot :growl :headbutt :heal-bell :hidden-power :last-resort :light-screen :lucky-chant :magic-coat :metronome :mirror-move :mud-slap :nasty-plot :natural-gift :peck :present :protect :psych-up :psychic :psycho-shift :rain-dance :reflect :rest :return :rock-smash :rollout :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-kiss :swift :thunder-wave :toxic :trick :uproar :water-pulse :wish :yawn :zen-headbutt :helping-hand :tri-attack]
  :togetic [:aerial-ace :air-cutter :brick-break :defog :drain-punch :fly :focus-punch :giga-impact :heat-wave :hyper-beam :magical-leaf :ominous-wind :roost :silver-wind :steel-wing :tailwind :twister]
  :togekiss [:air-slash :aura-sphere :extremespeed :pluck :sky-attack])

(deflearnset :bw
  :togepi [:after-you :ancientpower :attract :baton-pass :bestow :charm :covet :double-edge :double-team :dream-eater :echoed-voice :encore :endeavor :extrasensory :facade :fire-blast :flamethrower :flash :fling :follow-me :foresight :frustration :future-sight :grass-knot :growl :heal-bell :hidden-power :hyper-voice :incinerate :last-resort :light-screen :lucky-chant :magic-coat :metronome :mirror-move :morning-sun :nasty-plot :peck :present :protect :psych-up :psychic :psycho-shift :psyshock :rain-dance :reflect :rest :return :rock-smash :round :safeguard :secret-power :shadow-ball :signal-beam :sleep-talk :snore :solarbeam :stored-power :substitute :sunny-day :swagger :sweet-kiss :telekinesis :thunder-wave :toxic :trick :uproar :wish :work-up :yawn :zen-headbutt :helping-hand :tri-attack]
  :togetic [:aerial-ace :brick-break :drain-punch :fly :giga-impact :heat-wave :hyper-beam :magical-leaf :retaliate :roost :tailwind]
  :togekiss [:air-slash :aura-sphere :extremespeed :pluck :sky-attack])

