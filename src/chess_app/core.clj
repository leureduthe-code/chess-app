(ns chess-app.core
  (:require [chess-app.chess :as chess]
            [chess-app.utils :as utils]))



(def current-player (atom :white))

(defn switch-turn
  "switchs current player when called"
  []
(if (= @current-player :white)
(reset! current-player :black)
(reset! current-player :white)))







(defn get-in-pos
  "get what is at position in board from chess notation"
  [board notation]
  (let [parsed (utils/parse-pos notation)
        _ (assert (not= :error parsed) (str "notation must result in valid position : " notation))
        pos (utils/parsed-pos->keyword-pos parsed) 
        coord (utils/get-coord pos)] 
    (if (not-any? nil? coord)
     (get-in board [(first coord) (second coord)])
      (utils/error-msg "Cannot retrieve for notation :" notation))
    ))

(defn put-in-pos [board from to piece]
  (let [parsed-to (utils/parse-pos to)
        parsed-from (utils/parse-pos from)
        _ (assert (and (not= :error parsed-from) (not= :error parsed-to)) (str "notation must result in valid position : " parsed-to parsed-from))
        pos-to (utils/parsed-pos->keyword-pos parsed-to)
        pos-from (utils/parsed-pos->keyword-pos parsed-from)
        coord-to (utils/get-coord pos-to)
        coord-from (utils/get-coord pos-from)]
    (if (and (not-any? nil? coord-from) (not-any? nil? coord-to)) 
        (-> board
            (assoc-in  [(first coord-to) (second coord-to)] piece)
            (assoc-in  [(first coord-from) (second coord-from)] nil))
       (utils/error-msg "Cannot put for notation :" to))))



(defn move-to [board from to]
  (let [piece-to-move (get-in-pos board from) 
        piece-at-location (get-in-pos board to)] 
    (put-in-pos board from to piece-to-move)))






(def moves-function {
                     :p chess/pawn-moves
                     :P chess/pawn-moves
                     :r chess/rook-moves
                     :R chess/rook-moves
                     :N chess/knight-moves
                     :n chess/knight-moves
                     :b chess/bishop-moves
                     :B chess/bishop-moves
                     :Q chess/queen-moves
                     :q chess/queen-moves
                     :K chess/king-moves
                     :k chess/king-moves
                     })






(comment (for [row (range (count chess/board))
               col (range (count (nth chess/board row)))]
           (let [piece (get-in chess/board [col row])]))

         (doseq [row (range (count chess/board))
                 col (range (count (nth chess/board row)))]
           (println "Processing:" [col row]))


         ;;find all moves for white pawns
         (for [pos ["a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"]]
           (chess/pawn-moves chess/board pos :black))
         ;; does the same for black pawns
          (for [pos ["a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"]]
           (chess/pawn-moves chess/board pos :black))
         
          ;; counts the moves 
         (count (flatten (for [pos ["a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"]]
                           (chess/pawn-moves chess/board pos :black))))
         ;;find all moves for white knights
         (count (flatten (for [pos ["b1" "g1"]]
                           (chess/knight-moves chess/board pos :black))))
         )

(defn find-moves 
  "given a piece (ex :K) and a position (ex e4) gives the moves of that piece"
  [ board piece position opponent-color]
  (println piece position)
   ((piece moves-function) board position opponent-color))
  


(defn get-opponent-moves 
  "given the board and the opponent color , give all the possible moves of his pieces"
  [board opponent-color] 
  (->>(for [row (range (count board))
            col (range (count (nth board row)))]
        (let [piece (get-in board [col row])
              chess-notation (utils/coord->chess-notation [col row])]
           (when (some #{piece} (opponent-color chess/players)) ;; if the piece is opponent piece compute the moves and add them
             (find-moves board piece chess-notation opponent-color))))
      (filter #(not (nil? %)))
      (flatten)
      (set)))




(defn print-board-with-labels [board]
  (let [to-string (fn [cell]
                    (if cell
                      (name cell)
                      "."))
        columns "  a b c d e f g h"]
    (println)
    (doseq [[idx row] (map-indexed vector board)]
      (println (str (- 8 idx) "  " (apply str (interpose " " (map to-string row))))))
    (println)
    (print " ")
    (println columns)))


(defn present-moves [f & args]
  (print ">> You can play : ")
  (apply println (apply f args)))




(defn game-loop []
  (println)
  (println "Welcome to CHESS in Clojure")
  (println "Here is the board :")
  (print-board-with-labels chess/board)
  (println)
  (println "Enter the chess notation of a piece to know it's available moves (ex : h2)") 
  (let [position (read-line)
        piece (get-in-pos chess/board position)
        opponent-color (if (= :white @current-player) :black :white)] 
    (find-moves chess/board piece position opponent-color)
    
    
    ))




(defn -main [& args] 
  ;(game-loop)
  )

;; pb in sliding move generation 