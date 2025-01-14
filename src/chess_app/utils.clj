(ns chess-app.utils 
  (:require
   [chess-app.chess :as chess]))


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
  (list ((second keyword-pos) chess/chess-notation) ((first keyword-pos) chess/chess-notation)))

(defn coord->chess-notation [[row col]]
  (str ((keyword (str col)) chess/col-map)  ((keyword (str row)) chess/row-map)))

(defn chess-notation->vector [chess-notation]
  (-> chess-notation
      (parse-pos)
      (parsed-pos->keyword-pos)
      (get-coord)))
