
(in-ns 'smogon.dex)


(defpokemon :spinda
  :name "Spinda"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:own-tempo]
              :dp [:own-tempo :tangled-feet]
              :bw [:contrary :own-tempo :tangled-feet]]
  :stats [[60 60 60 60 60 60]]
  :weight [5]
  :height [1.1])

(deffamily (familychain :spinda))

(deflearnset :rs
  :spinda [:assist :attract :baton-pass :body-slam :brick-break :calm-mind :counter :defense-curl :dig :disable :dizzy-punch :double-edge :double-team :dream-eater :dynamicpunch :encore :endure :facade :faint-attack :fire-punch :flail :flash :focus-punch :frustration :hidden-power :hypnosis :ice-punch :icy-wind :mega-kick :mega-punch :metronome :mimic :mud-slap :nightmare :protect :psybeam :psych-up :psychic :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :skill-swap :sleep-talk :smellingsalt :snatch :snore :strength :substitute :sunny-day :swagger :swift :tackle :teeter-dance :thief :thrash :thunderpunch :toxic :trick :uproar :water-pulse :wish :body-slam :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :spinda [:assist :attract :baton-pass :brick-break :calm-mind :captivate :copycat :dig :disable :dizzy-punch :double-edge :double-team :drain-punch :dream-eater :encore :endure :facade :faint-attack :fake-out :fire-punch :flail :flash :fling :focus-punch :frustration :headbutt :helping-hand :hidden-power :hypnosis :ice-punch :icy-wind :last-resort :low-kick :mud-slap :natural-gift :protect :psybeam :psych-up :psychic :psycho-cut :rain-dance :recycle :rest :return :rock-slide :rock-smash :rock-tomb :role-play :rollout :safeguard :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :smellingsalt :snatch :snore :strength :substitute :sucker-punch :sunny-day :swagger :swift :tackle :teeter-dance :thief :thrash :thunderpunch :toxic :trick :trick-room :uproar :water-pulse :wish :zen-headbutt :body-slam :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :spinda [:assist :attract :baton-pass :brick-break :calm-mind :copycat :covet :dig :disable :dizzy-punch :double-edge :double-team :drain-punch :dream-eater :encore :facade :faint-attack :fake-out :fake-tears :fire-punch :flail :flash :fling :frustration :helping-hand :hidden-power :hyper-voice :hypnosis :ice-punch :icy-wind :last-resort :low-kick :protect :psybeam :psych-up :psychic :psycho-cut :rain-dance :rapid-spin :recycle :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :role-play :round :safeguard :shadow-ball :skill-swap :sleep-talk :smellingsalt :snatch :snore :strength :substitute :sucker-punch :sunny-day :swagger :tackle :teeter-dance :thief :thrash :thunderpunch :toxic :trick :trick-room :uproar :water-pulse :wild-charge :wish :work-up :zen-headbutt :body-slam :icy-wind :mimic :seismic-toss])
