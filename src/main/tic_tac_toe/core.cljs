(ns tic-tac-toe.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [cljs.reader :refer [read-string]]))

(defn cider-test
  []
  (prn "hello world!"))

 ;;tic-tac-toe ;;chess

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




;;CHESS:



(defonce chess-state (r/atom
                      {
                       
                       :piece-clicked nil
                       
                       :can-move-to nil 
                       
                       :board [[nil nil nil nil nil nil nil {:type :king :color :black}]
                               [nil nil nil nil nil nil nil nil]
                               [nil nil nil nil nil nil nil nil]
                               [nil nil nil nil nil nil nil nil]
                               [nil nil nil nil nil nil nil nil]
                               [nil nil nil nil nil nil nil nil]
                               [nil nil nil nil nil nil nil nil]
                               [{:type :king :color :black}
                                nil nil nil nil nil nil nil]]

                       }))

(comment

  (vector-2d 8 (range 64))
  #_([[00 01 02 03 04 05 06 07]
    ;;[08 09 10 11 12 13 14 15]
    [16 17 18 19 20 21 22 23]
    [24 25 26 27 KI 29 30 31]
    [32 33 34 35 36 37 38 39]
    [40 41 42 43 44 45 46 47]
    [48 49 50 51 52 53 54 55]
    [56 57 58 59 60 61 62 63]])
  )

(def chess-piece-data
  {:pawn {:name "pawn"
          :symbol :P
          :movement {:U 2}}
   
   :king {:name "king"
          :symbol :K
          :movement {:L 1
                     :R 1
                     :U 1
                     :D 1
                     :UL 1
                     :UR 1
                     :DL 1
                     :DR 1}
          #_(fn [board piece-index]
              [])}
   :rook {:name "king"
         }}
  
  )

(comment
  {:U 'n}
  {:U [> ]}
  
  

  )

(defn vector-2d
  [number-of-items-in-row  vector]
  (vec (map (fn [item]
              (vec item))
            (partition number-of-items-in-row vector )))
  
  )

(defn convert-to-coord 
  [number]
  (loop [yn number
         xcounter 0]
    (if (zero? (mod yn 8))
      [(/ yn 8) xcounter]
      (recur (- yn 1) (inc xcounter)))))

(defn read-dimensional-vector
  [cords vector]
  (let [cord-amount (count cords)]
    (loop [nested-vector vector
           counter 0]
      (if (= counter cord-amount)
        nested-vector
        (recur (nested-vector (cords counter)) (+ counter 1))))))

(comment

  {:U 4}
  
  (can-move-to [5 5] {:U 4} nil)
  (coord-direction-handler :U 4 [4 4])
  (can-move-to [0 0] {:L 1
                     :R 1
                     :U 1
                     :D 1
                      } 0)
  )

(defn coord-direction-handler
  [direction distance [y x]]
  (cond
    (= direction :U)
    [(- y distance) x]
    (= direction :D)
    [(+ y distance) x]
    (= direction :L)
    [y (- x distance)]
    (= direction :R)
    [y (+ x distance)]
    (= direction :UL)
    [(- y 1) (- x 1)]
    (= direction :UR)
    [(- y 1) (+ x 1)]
    (= direction :DL)
    [(+ y 1) (- x 1)]
    (= direction :DR)
    [(+ y 1) (+ x 1)]
    :else
    (prn "ERROR:" direction)
    )

  #_{:U (fn [[y x] n] [(- y n) x])
     :D (fn [[y x] n] [(+ y n) x])
     :L (fn [[y x] n] [y (- x n)])
     :R (fn [[y x] n] [y (+ x n)])})


(defn can-move-to
  [piece-coords piece-data board-state]
  (prn "piece-data: " piece-data)
  (vec  (mapcat (fn [[direction number-of-spaces]]
                  (map (fn [s]
                         (coord-direction-handler direction (+ 1 s) piece-coords))
                       (range number-of-spaces)))
                ((chess-piece-data (piece-data :type)) :movement)))
  )



(defn can-click-piece?
  [board-state piece-coords]
  (prn "can-click-piece? :" (not= (get-in board-state piece-coords) nil))
  (not= (get-in board-state piece-coords) nil))

(defn piece-clicked!
  [piece-coords piece-data board-state]
  (prn "SWAP piece-coords:" piece-coords)
  (prn "piece-data: " piece-data)
  (swap! chess-state (fn [state]
                       (-> state
                           (assoc :piece-clicked piece-coords)
                           (assoc :can-move-to (can-move-to piece-coords piece-data board-state)))))
  (prn ":can-move-to :" (@chess-state :can-move-to))
  )

(defn can-move-piece? 
  [space-coords piece-coords board-state]
  (let [move-piece? (and
                     (not (empty? (filter (fn [item]
                                            (= item space-coords))
                                          (@chess-state :can-move-to))))
                     (not= piece-coords nil))]
    (prn "can-move-piece? :" move-piece?)
    move-piece?)
  )

(defn move-piece!
  [board-state new-coords old-coords]
  (prn (into [:board] new-coords))
  (swap! chess-state
         (fn [state]
           (-> state
               (update-in (into [:board] new-coords)
                          (fn [bspace] ;;WILL NEED REWORKING
                            (get-in board-state old-coords)))
               (assoc :piece-clicked nil)
               (assoc :can-move-to nil)
               (assoc-in (into [:board] old-coords) nil)))))

(defn chess-square-on-click
  [piece-coords board-state]
  (let [board-state (@chess-state :board)
        last-piece-clicked (@chess-state :piece-clicked)]
    (prn "piece-coords: " piece-coords)
    (prn "square state: " (get-in @chess-state (into [:board] piece-coords))  )
    (prn "T1:" (get-in board-state piece-coords))
    (cond
      (can-click-piece? board-state piece-coords)
      (piece-clicked! piece-coords (get-in board-state piece-coords) board-state)
      
      (can-move-piece? piece-coords last-piece-clicked board-state)
      (move-piece! board-state piece-coords last-piece-clicked)
      
      )
    (prn "")))

(comment
  [[2 5] [2 7] [1 6] [3 6] [2 6]]
  (convert-to-coord 22)
  (not (empty? (filter (fn [item]
                         (= item [2 6]))
                      [[2 5] [2 7] [1 6] [3 6] [2 6]])))
  
  
  )


(defn square-color [index coord]
  (let [row (quot index 8)
        col (mod index 8) 
        sum (+ row col)]
    (cond
      (not (empty? (filter (fn [item]
                             (= item coord))
                           (@chess-state :can-move-to))))
      "yellow"
      (even? (mod sum 2))
      "white"
      :else
      "brown")))

(defn chess-square
  [i]
  (let [y&x (convert-to-coord i)]
    [:div {:style {:width "100px"
                   :height "100px"
                   :border "1px solid #333"
                   :display "flex"
                   :align-items "center"
                   :justify-content "center"
                   :font-size "2.5rem"
                   :font-weight "bold"
                   :cursor "pointer"
                   :background-color (square-color i y&x)}
           :on-click #(chess-square-on-click y&x (@chess-state :board))
           #_(when-not (@app-state :win) 
               (square-on-click! i))}
     
     (when-let [piece (get-in @chess-state (into [:board] y&x)) ]
       ((chess-piece-data (piece :type)):symbol))])
  )

(comment

  (into [:board]  (convert-to-coord 56)  )

  (get-in @chess-state (into [:board]  (convert-to-coord 56)  ))
  
  (get-in @chess-state [:board 7 0])

  (prn @chess-state)

  )


(defn board-chess
  []

  #_[chess-square 0]
  [:div 
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(8, 100px)"
                  :grid-template-rows "repeat(8, 100px)"
                  :gap "5px"
                  :background-color "#333"
                  :padding "5px"
                  :width "max-content"
                  :margin "20px auto"}}
    (for [i (range 64)]
      ^{:key i} [chess-square i])]
   ])


(comment
  (do    (defn convert-to-coords 
           [number]
           (loop [yn number
                  xcounter 0]
             (if (zero? (mod yn 8))
               [xcounter (/ yn 8)]
               (recur (- yn 1) (inc xcounter)))))
         
         (convert-to-coords 25))

  #_( \      0  1  2  3  4  5  6  7 — X       
      0    [00 01 02 03 04 05 06 07]
      1  ;;[08 09 10 11 12 13 14 15]
      2    [16 17 18 19 20 21 22 23]
      3    [24 25 26 27 KI 29 30 31]
      4    [32 33 34 35 36 37 38 39]
      5    [40 41 42 43 44 45 46 47]
      6    [48 49 50 51 52 53 54 55]                                                                     
      7    [56 57 58 59 60 61 62 63]
      |
      Y

     )
  

  (zero? (mod 10 8))
  
  (for [i (range 16)]
    (prn i)
    
    )

  )



(defn mount-root
  []
  #_(rdom/render [:h1 "test"] (.getElementById js/document "app"))
  #_(rdom/render [board-tic-tac-toe] (.getElementById js/document "app"))

  
  (rdom/render [board-chess] (.getElementById js/document "app")) ;;NEED TO CHANGE chess-square
  
  )

(defn init
  []
  (mount-root))


