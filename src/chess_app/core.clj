(ns chess-app.core)

(def board [[:r :n :b :q :k :b :n :r] ;8
            [:p :p :p :p :p :p :p :p] ;7
            [nil nil nil nil nil nil nil nil] ;6
            [nil nil nil nil nil nil nil nil] ;5
            [nil nil nil nil nil nil nil nil] ;4
            [nil nil nil nil nil nil nil nil] ;3
            [:P :P :P :P :P :P :P :P] ;2
            [:R :N :B :Q :K :B :N :R]]) ;1
            ;a  ;b ;c ;d ;e ;f ;g ;h

(def players {
                 :white [:r :n :b :q :k :p]
                 :black [:R :N :B :Q :K :P]
})

(def current-player (atom :white))

(defn switch-turn []
(if (= @current-player :white)
(reset! current-player :black)
(reset! current-player :white)))

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

(def piece-offsets
  {:king [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
   :queen (concat [[-1 0] [1 0] [0 -1] [0 1]] [[-1 -1] [-1 1] [1 -1] [1 1]])
   :rook [[-1 0] [1 0] [0 -1] [0 1]]
   :bishop [[-1 -1] [-1 1] [1 -1] [1 1]]
   :knight [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
   :white-pawn {:move [[-1 0]] :double-move [[-2 0]] :captures [[-1 -1] [-1 1]]}
   :black-pawn {:move [[1 0]] :double-move [[2 0]] :captures [[1 -1] [1 1]]}})                   

(defn legal-move-pawn [board pawn-pos]
(comment "one square in front if nil in front or one square lateraly if oponent piece in that lateral square"))

(defn legal-move-rook [board rook-pos]
(comment "any number of squares verticaly and horizontaly, blocked by same side pieces, captures oponent pieces"))



(defn error-msg [& args]
  (apply println args)
  :error)

(defn parse-pos
  "parse a chess pos to a sequence of string e4 -> '(e 4)"
  [chess-pos] 
  (if (= 2 (count chess-pos))
    (do
      (assert (-> (first chess-pos)
                  (char?)) (str chess-pos " is not a valid chess position"))
      (assert (-> (second chess-pos)
                  (str)
                  (Integer/parseInt)
                 (number?)) (str chess-pos " is not a valid chess position"))
       (for [x chess-pos]
         (str x)))
    (error-msg "not a valid chess notation !")))

(defn parsed-pos->keyword-pos
  "transform a parsed chess pos to a sequence of keyword '(e 4) -> '(:e :4)"
  [parsed-pos] 
  (map #(keyword %) parsed-pos))

(defn get-coord
  "turns keyword-pos in vector coord '(:e :1) -> '(7 4)"
  [keyword-pos]
  (list ((second keyword-pos) chess-notation) ((first keyword-pos) chess-notation)))

(defn chess-notation->vector [chess-notation]
(-> chess-notation
    (parse-pos)
    (parsed-pos->keyword-pos)
    (get-coord)))


(defn get-in-pos
  "get what is at position in board from chess notation"
  [board notation]
  (let [parsed (parse-pos notation)
        _ (assert (not= :error parsed) (str "notation must result in valid position : " notation))
        pos (parsed-pos->keyword-pos parsed) 
        coord (get-coord pos)] 
    (if (not-any? nil? coord)
     (get-in board [(first coord) (second coord)])
      (error-msg "Cannot retrieve for notation :" notation))
    ))

(defn put-in-pos [board from to piece]
  (let [parsed-to (parse-pos to)
        parsed-from (parse-pos from)
        _ (assert (and (not= :error parsed-from) (not= :error parsed-to)) (str "notation must result in valid position : " parsed-to parsed-from))
        pos-to (parsed-pos->keyword-pos parsed-to)
        pos-from (parsed-pos->keyword-pos parsed-from)
        coord-to (get-coord pos-to)
        coord-from (get-coord pos-from)]
    (if (and (not-any? nil? coord-from) (not-any? nil? coord-to)) 
        (-> board
            (assoc-in  [(first coord-to) (second coord-to)] piece)
            (assoc-in  [(first coord-from) (second coord-from)] nil))
       (error-msg "Cannot put for notation :" to))))



(defn move-to [board from to]
  (let [piece-to-move (get-in-pos board from) 
        piece-at-location (get-in-pos board to)] 
    (put-in-pos board from to piece-to-move)))



(defn compute-legal-moves [board-size offsets [current-row current-col]]
(->> offsets
      (map (fn [[offset-row offset-col]] [(+ offset-row current-row) (+ offset-col current-col)])) ; generates possible pos from offsets and current pos
      (filter (fn [[row col]] (and (>= row 0) (< row board-size) ; keeps only new pos with rows and cols that are in bound
                                   (>= col 0) (< col board-size))))))




(defn scale-vector [direction position scale]
  (let [[dx dy] direction
        [row col] position]
    [(+ row (* dx scale)) (+ col (* dy scale))]))


(defn generate-sliding-moves 
"generates all possibles moves for sliding pieces (rook queen bishop) including capturing moves. 
It takes the board , the current position of the piece ex [4 4] and directions offset of the piece
ex  [[-1 0] [1 0] [0 -1] [0 1]] for the rook "
[board position directions]
  (let [board-size 8
        in-bounds? (fn [[r c]] (and (>= r 0) (< r board-size)
                                    (>= c 0) (< c board-size)))]
    (reduce (fn [moves direction]
              (loop [scale 1
                     current-moves moves]
                (let [new-pos (scale-vector direction position scale)]
                  (if (not (in-bounds? new-pos))
                    current-moves ; Stop if out of bounds
                    (if (nil? (get-in board new-pos))
                      ; the position we are at is empty we had the move and continue looping
                      (recur (inc scale) (conj current-moves new-pos)) 
                      ; the position we are at is not empty
                      (let [piece-at-pos (get-in board new-pos)]
                      ; we check if it is a piece of the current player 
                        (if (some #{piece-at-pos} (@current-player players)) 
                        current-moves ;if yes we do not add the move and stop looping
                        (conj current-moves new-pos)))))))) ;else it is a capturing move and we add it and stop looping
                      [] ; start with empty move list
                      directions)))


(defn knight-moves [position]
  (let [offsets (:knight piece-offsets) 
        vector-pos (chess-notation->vector position)    
        [row col] vector-pos
        board-size 8]
    (compute-legal-moves board-size offsets [row col])))    


