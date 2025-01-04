(ns chess-app.core)

(def board [[:r :n :b :q :k :b :n :r]
            [:p :p :p :p :p :p :p :p]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil]
            [:P :P :P :P :P :P :P :P]
            [:R :N :B :Q :K :B :N :R]])

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

(defn parse-pos
  "parse a chess pos to a sequence of string e4 -> '(e 4)"
  [chess-pos]
  (for [x chess-pos]
   (str x)))

(defn parsed-pos->keyword-pos
  "transform a parsed chess pos to a sequence of keyword '(e 4) -> '(:e :4)"
  [parsed-pos]
  (map #(keyword %) parsed-pos))

(defn get-in-pos
  "get what is at position in board from chess notation"
  [board notation]
  (let [parsed (parse-pos notation)
        pos (parsed-pos->keyword-pos parsed)
        y ((first pos) chess-notation)
        x ((second pos) chess-notation)]
    (println x y)
    (get-in board [x y])))

(get-in-pos board "e1")


