
(in-ns 'smogon.dex)

(defpokemon :victini
  :name "Victini"
  :introduced-in :bw
  :types [[:fire :psychic]]
  :egggroups [[]]
  :abilities [[:victory-star]]
  :stats [[100 100 100 100 100 100]]
  :weight [4]
  :height [0.4])

(deffamily (familychain :victini))

(deflearnset :bw
  :victini [:blue-flare :bolt-strike :bounce :brick-break :charge-beam :confusion :double-edge :double-team :embargo :endure :energy-ball :facade :final-gambit :fire-blast :fire-punch :flame-burst :flame-charge :flamethrower :flare-blitz :flash :fling :focus-blast :focus-energy :frustration :fusion-flare :fusion-bolt :giga-impact :glaciate :grass-knot :headbutt :heat-wave :helping-hand :hidden-power :hyper-beam :incinerate :inferno :last-resort :light-screen :magic-coat :overheat :protect :psych-up :psychic :psyshock :quick-attack :rest :return :reversal :rock-smash :role-play :round :safeguard :searing-shot :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stored-power :substitute :sunny-day :swagger :taunt :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :uproar :v-create :wild-charge :will-o-wisp :work-up :zen-headbutt])
