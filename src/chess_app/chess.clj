(ns chess-app.chess
  (:require [chess-app.utils :as utils]))

(def board [[:r :n :b :q :k :b :n :r] ;8
            [:p :p :p :p :p :p :p :p] ;7
            [nil nil nil nil nil nil nil nil] ;6
            [nil nil nil nil nil nil nil nil] ;5
            [nil nil nil nil nil nil nil nil] ;4
            [nil nil nil nil nil nil nil nil] ;3
            [:P :P :P :P :P :P :P :P] ;2
            [:R :N :B :Q :K :B :N :R]]) ;1
            ;a  ;b ;c ;d ;e ;f ;g ;h

(def players {:black [:r :n :b :q :k :p]
              :white [:R :N :B :Q :K :P]})


(def chess-notation {:1 7
                     :2 6
                     :3 5
                     :4 4
                     :5 3
                     :6 2
                     :7 1
                     :8 0
                     :a 0
                     :b 1
                     :c 2
                     :d 3
                     :e 4
                     :f 5
                     :g 6
                     :h 7})

(def col-map {:0 "a"
              :1 "b"
              :2 "c"
              :3 "d"
              :4 "e"
              :5 "f"
              :6 "g"
              :7 "h"})

(def row-map {:7 1
              :6 2
              :5 3
              :4 4
              :3 5
              :2 6
              :1 7
              :0 8})

(def pawn-moves-status {; needed to know if this is the first time a pawn has moved
                        [:white  0] false
                        [:white  1] false
                        [:white  2] false
                        [:white  3] false
                        [:white  4] false
                        [:white  5] false
                        [:white  6] false
                        [:white  7] false
                        [:black  0] false
                        [:black  1] false
                        [:black  2] false
                        [:black  3] false
                        [:black  4] false
                        [:black  5] false
                        [:black  6] false
                        [:black  7] false})

(def piece-offsets
  {:king [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
   :queen (concat [[-1 0] [1 0] [0 -1] [0 1]] [[-1 -1] [-1 1] [1 -1] [1 1]])
   :rook [[-1 0] [1 0] [0 -1] [0 1]]
   :bishop [[-1 -1] [-1 1] [1 -1] [1 1]]
   :knight [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
   :white-pawn {:move [[-1 0]] :double-move [[-2 0]] :captures [[-1 -1] [-1 1]]}
   :black-pawn {:move [[1 0]] :double-move [[2 0]] :captures [[1 -1] [1 1]]}})                   


(defn opponent-piece?
  "Returns true if the piece belongs to the opponent, false otherwise."
  [piece opponent-color]
  (and piece ; Ensure the square is not empty
       (some #{piece} (opponent-color players)))) ; Check if it belongs to the opponent

(defn generate-legal-moves [board offsets [current-row current-col] opponent-color]
  (->> offsets
       (map (fn [[offset-row offset-col]] [(+ offset-row current-row) (+ offset-col current-col)])) ; generates possible pos from offsets and current pos 
       (filter (fn [[row col]] (and (>= row 0) (< row (count board)) ; keeps only new pos with rows and cols that are in bound
                                    (>= col 0) (< col (count board)))))
       (filter (fn [[row col]] ; prunes move that end on case with piece of current player
                 (let [piece-at-pos (get-in board [row col])]
                   (if (opponent-piece? piece-at-pos opponent-color)
                     true
                     false))))))




(defn scale-vector [direction position scale]
  (let [[dx dy] direction
        [row col] position]
    [(+ row (* dx scale)) (+ col (* dy scale))]))


(defn generate-sliding-moves
  "generates all possible moves for sliding pieces (rook, queen, bishop), including capturing moves. 
It takes the board, the current position of the piece (e.g., [4 4]), and direction offsets for the piece
(e.g., [[-1 0] [1 0] [0 -1] [0 1]] for the rook)."
  [board directions position opponent-color]
  (let [board-size (count board)
        in-bounds? (fn [[r c]] (and (>= r 0) (< r board-size)
                                    (>= c 0) (< c board-size)))]
    (reduce (fn [moves direction]
              (loop [scale 1
                     current-moves moves]
                (let [new-pos (scale-vector direction position scale)]
                  (if (not (in-bounds? new-pos))
                    current-moves ; Stop if out of bounds
                    (let [piece-at-pos (get-in board new-pos)]
                      (cond
                        (nil? piece-at-pos)
                        (recur (inc scale) (conj current-moves new-pos)) ; Add empty square and continue

                        (opponent-piece? piece-at-pos opponent-color)
                        (do
                          (println "Captured opponent piece at:" new-pos) ; Debug log
                          (conj current-moves new-pos)) ; Add capturing move and stop

                        :else
                        (do
                          (println "Friendly piece at:" new-pos "Stopping direction.") ; Debug log
                          current-moves))))))) ; Stop if it's a friendly piece) ; Stop if it is a friendly piece
            [] ; Start with empty moves list
            directions)))



(defn knight-moves
  "returns a list of legal moves for a knight at given position in chess notation"
  [board position opponent-color]
  (let [offsets (:knight piece-offsets)
        vector-pos (utils/chess-notation->vector position)
        [row col] vector-pos]
    (->> (generate-legal-moves board offsets [row col] opponent-color)
         (map utils/coord->chess-notation))))

(defn rook-moves
  "returns a list of legal moves for a rook at given position in chess notation"
  [board position opponent-color]
  (let [offsets (:rook piece-offsets)
        vector-pos (utils/chess-notation->vector position)
        [row col] vector-pos]
    (->> (generate-sliding-moves board offsets [row col] opponent-color)
         (map utils/coord->chess-notation))))

(defn bishop-moves
  "returns a list of legal moves for a bishop at given position in chess notation"
  [board position opponent-color]
  (let [offsets (:bishop piece-offsets)
        vector-pos (utils/chess-notation->vector position)
        [row col] vector-pos]
    (->> (generate-sliding-moves board offsets [row col] opponent-color)
         (map utils/coord->chess-notation))))

(defn queen-moves
  "returns a list of legal moves for a queen at given position in chess notation"
  [board position opponent-color]
  (let [offsets (:queen piece-offsets)
        vector-pos (utils/chess-notation->vector position)
        [row col] vector-pos]
    (->> (generate-sliding-moves board offsets [row col] opponent-color)
         (map utils/coord->chess-notation))))

(defn king-moves
  "returns a list of legal moves for a queen at given a board and his position in chess notation"
  [board position opponent-color]
  ;(println "Work in progress")
  )


(defn pawn-moves
  "returns a list of legal moves for a pawn at given position in chess notation"
  [board position opponent-color]
  (let [vector-pos (utils/chess-notation->vector position)
        piece (get-in board vector-pos)
        [row col] vector-pos
        move-lib (if (some #{piece} (:white players)) (:white-pawn piece-offsets) (:black-pawn piece-offsets))
        simple-move (:move move-lib)
        double-move (:double-move move-lib)
        capture-move (:captures move-lib)
        player (if (= opponent-color :black) :white :black)]

    (->> (concat
          (generate-legal-moves board simple-move [row col] opponent-color) ; generates regular moves
          (filter (fn [[row col]] (and (opponent-piece? (get-in board [row col]) opponent-color) (not (nil? (get-in board [row col]))))) (generate-legal-moves board capture-move [row col] opponent-color)) ;prune if no capture possible
          (when (not (get pawn-moves-status [player col])) ; when the pawn has not moved yet it generate the double move available
            (generate-legal-moves board double-move [row col] opponent-color)))
         (map utils/coord->chess-notation))))

