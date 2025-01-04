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

(defn error-msg [& args]
  (apply println args)
  :error)

(defn parse-pos
  "parse a chess pos to a sequence of string e4 -> '(e 4)"
  [chess-pos]
  (println chess-pos)
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

(defn get-in-pos
  "get what is at position in board from chess notation"
  [board notation]
  (let [parsed (parse-pos notation)
         _ (assert (not= :error parsed) (str "notation must result in valid position : " notation))
        pos (parsed-pos->keyword-pos parsed) 
        y ((first pos) chess-notation)
        x ((second pos) chess-notation)] 
    (if-not (or (= nil x) (= nil y))
      (get-in board [x y])
      (error-msg "Cannot retrieve for notation :" notation))
    ))

(get-in-pos board "e1")


