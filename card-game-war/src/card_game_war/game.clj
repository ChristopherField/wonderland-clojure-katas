(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn build-ranked-map [list]
  (into {}
        (map-indexed
         (fn [idx item]
           [item idx])
         list)))
(def suit-map (build-ranked-map suits))
(def rank-map (build-ranked-map ranks))
(defn extract-card-magnitude-vector
  [card]
  (let [rank-mag (rank-map (card 1))
        suit-mag (suit-map (card 0))]
    [suit-mag rank-mag]))

(defn magnitude-compare [item1 item2]
  (cond
   (< item1 item2) 1
   (> item1 item2) 0
   :true nil))

(defn compare-card [card1 card2]
  (let [mags [(extract-card-magnitude-vector card1) (extract-card-magnitude-vector card2)]
        rank-comp (apply magnitude-compare (map #(% 1) mags))]
    (if rank-comp
      rank-comp
      (apply magnitude-compare (map #(% 0) mags)))))

(defn return-winner-if-exist
  [player1-card player2-card]
  (if (nil? player1-card)
    1
    (if (nil? player2-card)
      0
      nil)))


(defn play-round [player1-card player2-card]
  (compare-card player1-card player2-card))


(defn play-game [player1-cards player2-cards]
  (let [player1-card (first (seq player1-cards))
        player2-card (first (seq player2-cards))
        winner (return-winner-if-exist player1-card player2-card)]
  (if winner
    winner
    (let [round (play-round player1-card player2-card)]
      ;;(print "Round: " round " winner " winner "player1: " player1-cards " player2: " player2-cards "\n")
      (if (= round 0)
        (recur (lazy-cat (rest player1-cards) [player1-card player2-card]) (rest player2-cards))
        (recur (rest player1-cards) (lazy-cat (rest player2-cards) [player1-card player2-card])))))))
