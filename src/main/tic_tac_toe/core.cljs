(ns tic-tac-toe.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [cljs.reader :refer [read-string]]))

(defonce app-state
  (r/atom
   {    
    :current-player :X
    :bot? false
    
    :game-end-text nil
    :win false

    :gravity? false
    
    :board [nil nil nil
            nil nil nil
            nil nil nil]}))

(defonce ws 
  (let [socket (js/WebSocket. "ws://wabbit.bumble.fish:8080")]
    (set! (.-onmessage socket) (fn [event]
                                 (let [new-state (read-string (.-data event))]
                                   (prn "new-state: " new-state)
                                   (reset! app-state new-state))))
    socket))

(comment
  (random-bot
   [:X nil nil
    :X  nil nil
    nil :O nil]))

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
      (recur (+ i 3)))))


(comment
  (possible-moves [nil nil nil
                   nil nil nil
                   nil :X nil]
                  true
                  )

  (possible-moves [nil nil :X
                   :X  nil :O
                   :X  nil :O]
                  true)
  )

(defn possible-moves
  [board gravity?]
  (let [empty-indices (fn [b]
                        (keep-indexed (fn [index item]
                                        (when (nil? item) index))
                                      b))]
    (vec (if gravity?
           (map #(gravity % board) (empty-indices (subvec board 0 3)))
           (empty-indices board)))))

(defn random-bot
  [board]
  (let [moves (possible-moves board (@app-state :gravity?))       
        chosen-move (rand-int (count moves))]
    (moves chosen-move)))

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
                     :bot? (@app-state :bot?)
                     :winner-text nil
                     :win false

                     :gravity? gravity? 
                     
                     :board [nil nil nil
                             nil nil nil
                             nil nil nil]}))

(defn add-letter-to-board!
  [i]
  (prn i)
  
  (swap! app-state assoc-in [:board i] (@app-state :current-player)))

(defn switch-player!
  []
  (swap! app-state update :current-player (fn [c]
                                            (if (= c :X)
                                              :O
                                              :X))))

(defn square-on-click!
  [i]
  (prn i)
  (let [square-nil? (nil? ((@app-state :board) i ))]
    (when square-nil?
      (do (add-letter-to-board!
           (if (@app-state :gravity?)
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
                 :background-color "white"}
         :on-click #(when-not (@app-state :win)
                      (square-on-click! i))}
   
   ((-> @app-state
        :board ) i)
   
   #_(if (even? 
          )
       "#000000"
       "#fff"
       )
         
   #_((square-color i)) 
   #_((-> @app-state
          :board ) i)])

(defn board-tic-tac-toe
  []
  (when (and (@app-state :bot?)
             (= :O (@app-state :current-player))
             (not (@app-state :win)))
    (square-on-click! (random-bot (@app-state :board))))
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
      ^{:key i} [square i])]
   
   [:div {:style {:margin-top "40px"
                  :text-align "center"}}
    [:button {:on-click #(.send ws @app-state)} "transfer state"]
    [:div {:style {:margin-top "20px"}}]]])


(defonce chess-state (r/atom
                      {
                       :board-test [nil  nil nil nil
                                    nil nil nil nil
                                    nil nil nil nil
                                    :king  nil nil nil]

                       :piece-clicked nil
                       
                       
                       :board [:R  :N  :B  :Q  :K  :B  :N  :R
                               :P  :P  :P  :P  :P  :P  :P  :P
                               nil nil nil nil nil nil nil nil
                               nil nil nil nil nil nil nil nil
                               nil nil nil nil nil nil nil nil
                               nil nil nil nil nil nil nil nil
                               nil nil nil nil nil nil nil nil
                               nil nil nil nil nil nil nil nil]

                       }))
(def chess-pieces
  {:pawn {:name "pawn"
          :symbol :P
          :movement {:move-square {:up 1
                                   :right 0
                                   :left 0
                                   :down 0}}}
   :king {:name "king"
          :symbol :K
          :movement {:move-square {:up 1
                                   :right 1
                                   :left 1
                                   :down 1}}}})

(defn act-2d-vector
  [vector ylimit ])

(comment
  
  (vec (partition 4 [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]))
  [0 1 2 3 4 5 6 7]
  [[0 1 2 3]
   [4 5 6 7]]
  
  ;;chess piece data structure
  {:name "pawn"
   :symbol :P
   :movement {:move-square {:up 1
                            :up-right 0
                            :up-left 0

                            :down 0
                            :down-right 0
                            :down-left 0
                            
                            :right 0
                            :left 0
                            }}
   #_(:capturing {})
   }
  )

(defn chess-square-on-click
  [i board-state]
  (prn "square clicked: " i)
  (prn "square state:" ((@chess-state :board-test) i) )
  (let [piece-clicked (@chess-state :piece-clicked)]
    (cond
      (not= (board-state i) nil)
      (do (prn "SWAP piece-clicked:" i)
          (swap! chess-state assoc :piece-clicked i))
      
      (not= piece-clicked nil)
      (do
        (prn "")
        (swap! chess-state (fn [state]
                             (-> state
                                 (assoc-in  [:board-test piece-clicked] nil)
                                 (update-in [:board-test i] (fn [bspace] ;;WILL NEED REWORKING
                                                              ((@chess-state :board-test) piece-clicked)
                                                              )))) )) 
      :else
      nil
      ))
  
  
  )

(defn square-color [index]
  (let [row (quot index 4) ;; integer division
        col (mod index 4)  ;; remainder
        sum (+ row col)]
    (if (even? (mod sum 2))
      "white"
      "brown")))

(defn chess-square
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
                 :background-color (square-color i)}
         :on-click #(chess-square-on-click i (@chess-state :board-test))
         #_(when-not (@app-state :win) 
             (square-on-click! i))}
   (when-let [x ((@chess-state :board-test) i)]
     ((chess-pieces x) :symbol))]
  )



(defn board-chess
  []
  [:div
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(4, 100px)"
                  :grid-template-rows "repeat(4, 100px)"
                  :gap "5px"
                  :background-color "#333"
                  :padding "5px"
                  :width "max-content"
                  :margin "20px auto"}}
    (for [i (range 16)]
      ^{:key i} [chess-square i])]
   ])




(defn mount-root
  []
  #_(rdom/render [board-tic-tac-toe] (.getElementById js/document "app"))
  (prn :foo1)
  (rdom/render [board-chess] (.getElementById js/document "app")))

(defn init
  []
  (mount-root))


