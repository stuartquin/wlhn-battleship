(ns battleship.core)

(def boardA [
  :Q :Q :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
])

(def boardB [
  :Q :Q :_ :Q :_ :Q :_ :_ :_ :Q
  :_ :_ :_ :Q :_ :Q :_ :_ :_ :Q
  :Q :_ :_ :Q :_ :Q :_ :_ :_ :Q
  :Q :_ :_ :_ :_ :_ :_ :_ :_ :Q
  :Q :_ :_ :_ :_ :_ :_ :_ :_ :_
  :Q :_ :Q :Q :_ :Q :_ :_ :_ :_
  :_ :_ :_ :_ :_ :Q :_ :Q :Q :Q
  :Q :Q :_ :_ :_ :_ :_ :_ :_ :_
  :_ :_ :_ :_ :_ :_ :_ :_ :_ :_
  :Q :Q :Q :Q :Q :_ :_ :_ :_ :_
])

(def *column-nb* 10)

(defn c1dto2d [column-nb i]
  (vector (mod i column-nb) (int (/ i column-nb))))

(defn c2dto1d* [column-nb v]
  (let [[x y] v]
    (+ x (* column-nb y))))

(defn generate-line [n]
  (apply str "+" (repeat n "---+")))

(defn render-board [column-nb raw-nb board-state]
  (let [line (generate-line column-nb)
        pieces-pos board-state ;(into {} board-state)
        ]
    (apply str "\n" line "\n"
           (map #(let [pos (c1dto2d column-nb (dec %))
                       c (name (get pieces-pos (dec %) " "))]
                   (if (zero? (mod % column-nb))
                           (format "| %s |\n%s\n" c line)
                           (format "| %s " c))) (range 1 (inc (* column-nb raw-nb)))))))

(def c2dto1d (partial c2dto1d* *column-nb*))

(defn attack-board 
  [board coord]
  (if (coll? coord)
    (let [d1 (c2dto1d coord)
          c (get board d1)
          n (if (= c :Q) :* :X)
          result-board (assoc board d1 n)
          game-over (not (some #(= :Q %) result-board))]
      [result-board game-over])
    [board true]))

(defn main-loop [board1 board2 player decision]
  (let [board (if player board1 board2)
        other-board (if player board2 board1)
        _ (println (render-board *column-nb* *column-nb* other-board))
        _ (println (str "Player " (if player "A" "B") ":"))
        move (decision board)
        [result game-over] (attack-board board move)]

    (if game-over
      (println (str "Game Over" "\n" (render-board *column-nb* *column-nb* result)))
      (if player
        (recur result board2 (not player) decision)
        (recur board1 result (not player) decision)))))

(defn user-input [board]
  (read-string (read-line)))

(main-loop boardA boardB true user-input)
