
(ns tic-tac-toe.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]))

(defonce app-state
  (r/atom
   {:current-player :X
    
    :winner-text nil
    :win false

    :gravity? false
    
    :board [nil nil nil
            nil nil nil
            nil nil nil]}))

(def wins
  [[0 1 2] [3 4 5] [6 7 8] ;; rows
   [0 3 6] [1 4 7] [2 5 8] ;; cols
   [0 4 8] [2 4 6]])       ;; diags

(defn three-in-a-row?
  [board]
  (let [player (@app-state :current-player)]
    (some (fn [indices]
            (every? #(= player (board %)) indices))
          wins)))

(defn reset-app-state!
  [current-player gravity?]
  (reset! app-state {:current-player current-player

                     :winner-text nil
                     :win false

                     :gravity? gravity? 
                     
                     :board [nil nil nil
                             nil nil nil
                             nil nil nil]}))


(defn is-letter-below?
  [index board]
  (if (> index 5)
    true
    (let [letter-below (board (+ index 3))]
      (if (= nil letter-below)
        false
        true))))

(defn gravity
  [index board]
  (loop [i index]
    (if (is-letter-below? i board)
      i
      (recur (+ i 3)))
    )
  )

(defn add-letter-to-board!
  [i]
  (swap! app-state assoc-in [:board i] (@app-state :current-player))
    
    
  )

(defn switch-player!
  []
  (swap! app-state update :current-player (fn [c]
                                            (if (= c :X)
                                              :O
                                              :X)
                                            
                                            #_(cond
                                                (= c :X)
                                                :O
                                                (= c :O)
                                                :C
                                                (= c :C)
                                                :X))))

(defn square-on-click!
  [i]
  (prn (@app-state :gravity?))
  (let [square-nil? (nil? ((@app-state :board) i ))]

    (when square-nil?
      (do (add-letter-to-board! (if (@app-state :gravity?)
                                  (gravity i (@app-state :board))
                                  i))
          (when (three-in-a-row? (@app-state :board) )
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
         :on-click #(when-not (@app-state :win)
                      (square-on-click! i))}
   ((-> @app-state
        :board ) i)])

(defn board
  []
  [:div
   [:h1 {:on-click (fn []
                     (prn "gravity? swapped")
                     (swap! app-state update :gravity?
                            (fn [b]
                              (not b))))}
    "Tic Tac Toe"]
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

   [:h2 {:style {:display "flex"
                 :justify-content "center"
                 :align-items "center"
                 :color "blue"}
         :on-click #(reset-app-state! (@app-state :current-player) (@app-state :gravity?))}
    "click to reset"]
   
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
