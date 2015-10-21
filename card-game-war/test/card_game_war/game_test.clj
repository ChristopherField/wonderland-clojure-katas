(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0 (play-round [:heart 4] [:heart 3]))))
  (testing "queens are higher rank than jacks"
    (is (= 1 (play-round [:heart :jack] [:heart :queen]))))
  (testing "kings are higher rank than queens"
    (is (= 0 (play-round [:heart :king] [:heart :queen]))))
  (testing "aces are higher rank than kings"
    (is (= 1 (play-round [:spade :king] [:spade :ace]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= 1 (play-round [:spade :king] [:club :king]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= 0 (play-round [:diamond 2] [:club 2]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= 1 (play-round [:diamond 2] [:heart 2])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (= 1 (play-game [] [[:spade :king]])))
  (testing "a normal game where player 1 wins"
    (= 1 (play-game [[:spade 2] [:heart 3] [:spade :jack]] [[:club 2] [:heart 4] [:spade :ace]])))
  (testing "a normal game where player 0 wins"
    (= 0 (play-game [[:spade 4] [:spade :jack] [:spade :ace] [:heart :ace] [:diamond :ace] [:spade :queen] [:diamond :queen]]
                    [[:spade 5] [:spade 6] [:diamond :jack] [:spade 3] [:spade 2] [:diamond 2] [:diamond 3]]))))

