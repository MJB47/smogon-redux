(ns smogon.gaybot
    (:use clojure.string)
    (:require [smogon.dex :as dex])
    (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader)))

(def synirc {:name "irc.synirc.net" :port 6667})
(def user {:name "Smogon Pokedex" :nick "SmogDex"})


(declare conn-handler)


(defn connect 
    "connects to the server, returns a bunch of information that needs to be 
    passed around a bunch."
    [server]
    (let [socket (Socket. (:name server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))
        conn (ref {:in in :out out})]
    (doto (Thread. #(conn-handler conn)) (.start))
    conn))

(defn write 
    "sends message to the IRC server"
    [conn msg]
    (doto (:out @conn)
        (.println (str msg "\r"))
        (.flush)))

(defn message 
    "Sends a PRIVMSG so other users can see.
    This is the main way of communication"
    [conn dest msg]
    (write conn (str "PRIVMSG " dest " :" msg)))

(defn data
    "returns a bunch of useful information about a pokemon"
    [poke]
    (let [kpoke (keyword poke)]
    (if (dex/pokemon? kpoke)
        (str (clojure.string/capitalize poke) ": Typing: " (dex/type-of kpoke) " | Abilities: " 
            (dex/abilities-of kpoke) " | Stats: " (dex/hp-of kpoke) "/" 
            (dex/atk-of kpoke) "/" (dex/def-of kpoke) "/" (dex/spatk-of kpoke) 
            "/" (dex/spdef-of kpoke) "/" (dex/speed-of kpoke)) 
        (str poke " is not a valid pokemon, did you forget to hyphenate? (-)"))))

(defn learn 
    "tests whether a pokemon can learn a move or not"
    [poke move]
    (if (and (dex/pokemon? (keyword poke)) (dex/move? (keyword move)))
        (if-not (nil? (find (dex/has-move (keyword move)) (keyword poke))) 
            (str (clojure.string/capitalize poke) " DOES learn the move: " (clojure.string/capitalize move)) 
            (str (clojure.string/capitalize poke) " DOES NOT learn the move: " (clojure.string/capitalize move)))
        (str "Not a valid pokemon or move (or both!). Did you forget to hypenate? (-)")))

; need to return: Channel, Sender, Login, Hostname, Message
(defn on-message 
  [conn msg]
  (let [chan (nth (clojure.string/split msg #"\s+") 2)
        pmsg (nth (clojure.string/split msg #":+" 3) 2 nil)
        smsg (clojure.string/split pmsg #"\s")]
    (cond 
        (= "?hello" pmsg)
        (message conn chan "sup")
        (= "?data" (nth smsg 0 nil))
        (message conn chan (data (nth smsg 1 nil)))
        (= "?learn" (nth smsg 0 nil))
        (message conn chan (learn (nth smsg 1 nil) (nth smsg 2 nil))))))

; can join multiple channels with the syntax "#showdown,#trivia"
(defn join-chan 
    [conn chan]
    (write conn (str "JOIN " chan)))

(defn conn-handler 
    [conn]
    (while (nil? (:exit @conn))
        (let [msg (.readLine (:in @conn))]
            ;(println msg) ; use this when you want to see what the bot see's. Debugging
            (cond 
                (= (nth (clojure.string/split msg #"\s+") 1) "001")
                (join-chan conn "#penis")
                (= (nth (clojure.string/split msg #"\s+") 1) "PRIVMSG") 
                (on-message conn msg)
                (= (nth (clojure.string/split msg #"\s+") 1) "INVITE")
                (join-chan conn (nth (clojure.string/split msg #":+" 3) 2))
                (re-find #"^ERROR :Closing Link:" msg) 
                (dosync (alter conn merge {:exit true}))
                (re-find #"^PING" msg)
                (write conn (str "PONG "  (re-find #":.*" msg)))))))

(defn login 
    [conn user]
    (write conn (str "NICK " (:nick user)))
    (write conn (str "USER " (:nick user) " 0 * :" (:name user))))

(defn kill 
    [conn]
    (write conn "QUIT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-gaybot
    "Do not call this directly, instead call it from 
    smogon.core"
    []
    (let [irc (connect synirc)]
        (login irc user))
    (println "Starting up gaybot, may take time to connect"))

