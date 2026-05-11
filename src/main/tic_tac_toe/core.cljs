(ns tic-tac-toe.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]))

(defonce app-state
  (r/atom
   {:current-player :X

    :winner-text nil

    :win false
    
    :board [nil nil nil
            nil nil nil
            nil nil nil]}))

(defn three-in-a-row?
  []
  (let [board (@app-state :board)
        player (@app-state :current-player)
        row1? (and (= player (board 0))
                   (= player (board 1))
                   (= player (board 2)))
        row2? (and (= player (board 3))
                   (= player (board 4))
                   (= player (board 5)))
        row3? (and (= player (board 6))
                   (= player (board 7))
                   (= player (board 8)))
        colm1? (and (= player (board 0))
                    (= player (board 3))
                    (= player (board 6)))
        colm2? (and (= player (board 1))
                    (= player (board 4))
                    (= player (board 7)))
        colm3? (and (= player (board 2))
                    (= player (board 5))
                    (= player (board 8)))
        dia1? (and (= player (board 0))
                   (= player (board 4))
                   (= player (board 8)))
        dia2? (and (= player (board 2))
                   (= player (board 4))
                   (= player (board 6)))
        three-in-row? (or row1? row2? row3? colm1? colm1? colm2? colm3? dia1? dia2?)
        ]
    (prn row1? row2? row3? colm1? colm1? colm2? colm3? dia1? dia2?)
    three-in-row?
    )
  )

(defn evaluate-board
  [])

(defn add-letter-to-board!
  [i]
  (swap! app-state assoc-in [:board i] (@app-state :current-player)))

(defn switch-player!
  []
  (swap! app-state update :current-player (fn [c]
                                              (if (= c :X)
                                                :O
                                                :X))))

(defn square-on-click!
  [i]
  (let [square-nil? (nil? ((@app-state :board) i ))]

    (when square-nil?
      (do (add-letter-to-board! i)
          (when (three-in-a-row?)
            (swap! app-state (fn [state]
                               (-> state
                                   (assoc :winner-text (str "THE WINNER IS " (@app-state :current-player) "!!"))
                                   (assoc :win true)))))          
          (switch-player!) ))))

(defn square
  [i]
  [:div {:style {:width "100px"
                 :height "100px"
                 :border "1px solid #333"
                 :display "flex"
                 :align-items "center"
                 :justify-content "center"
                 :font-size "2.5rem"
                 :font-weight "bold"
                 :cursor "pointer"
                 :background-color "#fff"}
         :on-click #(square-on-click! i)}
   ((-> @app-state
        :board ) i)])

(defn board
  []
  [:div
   [:h1 "Tic Tac Toe"]
   [:h1 {:style {:display "flex"
                 :justify-content "center"
                 :align-items "center"}}
    (str (if (@app-state :win)
           "next"
           "current")
         "-player:") (@app-state :current-player) ]
   [:h1 {:style {:display "flex"
                 :justify-content "center"
                 :align-items "center"}}
    (@app-state :winner-text)]
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(3, 100px)"
                  :grid-template-rows "repeat(3, 100px)"
                  :gap "5px"
                  :background-color "#333"
                  :padding "5px"
                  :width "max-content"
                  :margin "20px auto"}}
    (for [i (range 9)]
      ^{:key i} [square i])]])

(defn mount-root
  []
  (rdom/render [board] (.getElementById js/document "app")))

(defn init
  []
  (mount-root))
