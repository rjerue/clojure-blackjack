(ns blackjack.core
  (:gen-class)
  (:require [clojure.string :as s]))

(def suits [:spade :clubs :hearts :diamonds])
(def card-values (into [10 10 10] (range 1 11)))

(defn make-card [suit value] {:suit suit :value value})
(defn make-deck  "Makes a deck of cards"
  []
  (-> (for [suit suits value card-values] (make-card suit value)) shuffle))

(defn sum-11 [deck]
  (reduce #(+ %1 (let [value (%2 :value)]
                   (if (= value 1) ;; replace 1s with 11s
                     11
                     value))) 0 deck))

(defn sum-cards [deck]
  (let [sum1 (reduce #(+ %1 (%2 :value)) 0 deck)
        sum11 (sum-11 deck)]
    (if (= 21 sum11)
      21
      sum1)))

(defn deck-string [deck]
  (s/join  ", " (reduce #(conj %1 (str (name (%2 :suit)) "-" (%2 :value))) [] deck)))

(defn formatted-message "Prints formatted message" [player dealer bet]
  (println "-----")
  (println (str "Your hand " (deck-string player) " sum: " (sum-cards player)))
  (println (str "Dealer hand " (deck-string dealer) " sum: " (sum-cards dealer)))
  (println (str "Bet " bet))
  (println "-----"))

(defn is-blackjack-winner? "Checks to see if there is a winner" [player dealer]
  (let [player-sum (sum-cards player) dealer-sum (sum-cards dealer)]
    (cond
      (= 21 player-sum dealer-sum) :tie
      (= 21 player-sum) :player
      (< 21 player-sum) :dealer
      :else (cond
              (= 21 dealer-sum) :dealer
              (< 21 dealer-sum) :player
              :else :none))))

(defn play-loop "play-loop" [player dealer deck bet d2]
  (formatted-message player dealer bet)
  (let [player-move #(play-loop (conj player (peek deck)) dealer (pop deck) bet d2)
        dealer-move #(play-loop player (conj dealer (if (nil? d2) (peek deck) d2)) (if (nil? d2) (pop deck) deck) bet nil)
        winner (is-blackjack-winner? player dealer)
        player-win #(do
                      (println "Player Wins")
                      :player)
        dealer-win #(do
                      (println "Dealer Wins")
                      :dealer)
        tie #(do (println "It's a tie") winner)
        dealer-sum (sum-cards dealer)
        player-sum (sum-cards player)]
    (case winner
      :player (player-win)
      :dealer (dealer-win)
      :tie (tie)
      (if (or (> (sum-cards dealer) 16) (> (sum-11 dealer) 16))
        (cond
          (> dealer-sum player-sum) (dealer-win)
          (< dealer-sum player-sum) (player-win)
          :else (tie))
        (if (nil? d2)
          (if (> dealer-sum player-sum) (dealer-win) (dealer-move))
          (do
            (println "Would you like to (h)it or (s)tand?")
            (case (read-line)
              "s" (dealer-move)
              "h" (player-move)
              (do (println "bad input?")
                  (play-loop player dealer deck bet d2)))))))))

(defn game-loop "the game with bets" [money]
  (if (> money 0)
    (do
      (println (str "You have $" money))
      (println "Place your bet:")
      (try
        (let [bet (. Integer parseInt (read-line)) [d1 p1 d2 p2 & rest-deck] (make-deck)]
          (if (> bet money)
            (do
              (println "You bet too much!")
              (game-loop money))
            (game-loop (case (play-loop [p1 p2] [d1] (vec rest-deck) bet d2)
                         :tie money
                         :dealer (- money bet)
                         :player (+ money bet)))))
        (catch Exception e ;; ex-info
          (println "Bad input")
          (game-loop money))))
    (println "You're broke. Goodnight!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Welcome to blackjack!")
  (game-loop 1000))

