
(in-ns 'smogon.dex)


(defpokemon :vulpix
  :name "Vulpix"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire]
              :bw [:drought :flash-fire]]
  :stats [[38 41 40 65 65]
          :gs [38 41 40 50 65 65]]
  :weight [9.9]
  :height [0.6])

(defpokemon :ninetales
  :name "Ninetales"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire]
              :bw [:drought :flash-fire]]
  :stats [[73 76 75 100 100]
          :gs [73 76 75 81 100 100]]
  :weight [19.9]
  :height [1.1])

(deffamily (familychain :vulpix :ninetales))



(deflearnset :rb
  :vulpix [:bide :body-slam :confuse-ray :dig :double-edge :double-team :ember :fire-blast :fire-spin :flamethrower :mimic :quick-attack :rage :reflect :rest :roar :skull-bash :substitute :swift :tail-whip :take-down :toxic]
  :ninetales [:hyper-beam])

(deflearnset :gs
  :vulpix [:attract :confuse-ray :curse :dig :disable :double-team :ember :endure :faint-attack :fire-blast :fire-spin :flail :flamethrower :frustration :headbutt :hidden-power :hypnosis :iron-tail :protect :quick-attack :rest :return :roar :safeguard :sleep-talk :snore :spite :sunny-day :swagger :swift :tail-whip :toxic]
  :ninetales [:hyper-beam])

(deflearnset :rs
  :vulpix [:attract :body-slam :confuse-ray :dig :disable :double-edge :double-team :ember :endure :facade :faint-attack :fire-blast :fire-spin :flail :flamethrower :frustration :grudge :heat-wave :hidden-power :howl :hypnosis :imprison :iron-tail :mimic :overheat :protect :psych-up :quick-attack :rest :return :roar :safeguard :secret-power :sleep-talk :snore :spite :substitute :sunny-day :swagger :swift :tail-whip :toxic :will-o-wisp :body-slam :charm :double-edge :mimic]
  :ninetales [:hyper-beam :body-slam :double-edge :mimic])

(deflearnset :dp
  :vulpix [:attract :captivate :confuse-ray :dark-pulse :dig :disable :double-team :ember :endure :energy-ball :extrasensory :facade :faint-attack :fire-blast :fire-spin :flail :flamethrower :flare-blitz :frustration :grudge :headbutt :heat-wave :hidden-power :howl :hypnosis :imprison :iron-tail :natural-gift :ominous-wind :overheat :pain-split :payback :power-swap :protect :psych-up :quick-attack :rest :return :roar :role-play :safeguard :secret-power :sleep-talk :snore :spite :substitute :sunny-day :swagger :swift :tail-whip :toxic :will-o-wisp :zen-headbutt :body-slam :charm :double-edge :mimic]
  :ninetales [:calm-mind :dream-eater :giga-impact :hyper-beam :nasty-plot :solarbeam :body-slam :double-edge :mimic])

(deflearnset :bw
  :vulpix [:attract :captivate :confuse-ray :covet :dark-pulse :dig :disable :double-team :ember :energy-ball :extrasensory :facade :faint-attack :fire-blast :fire-spin :flail :flame-burst :flame-charge :flamethrower :flare-blitz :foul-play :frustration :grudge :heat-wave :hex :hidden-power :howl :hypnosis :imprison :incinerate :inferno :iron-tail :overheat :pain-split :payback :power-swap :protect :psych-up :quick-attack :rest :return :roar :role-play :round :safeguard :secret-power :sleep-talk :snore :spite :substitute :sunny-day :swagger :tail-slap :tail-whip :toxic :will-o-wisp :zen-headbutt :body-slam :charm :double-edge :mimic]
  :ninetales [:calm-mind :dream-eater :giga-impact :hyper-beam :nasty-plot :psyshock :solarbeam :body-slam :double-edge :mimic])

