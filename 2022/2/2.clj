(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

; as vector
(def input
  (vec (map str/trim-newline (str/split (slurp (first *command-line-args*)) #"\n"))))

; returns a tuple that represents the hands thrown
(defn parse-line [line]
  (let [parts (str/split line #"\s+")
        theirsNum (case (first parts)
                    "A" 0
                    "B" 1
                    "C" 2)
        mineNum (case (second parts)
                  "X" 0
                  "Y" 1
                  "Z" 2)
        mineStrategy (case (second parts)
                       "X" "lose"
                       "Y" "draw"
                       "Z" "win")]
    {:theirs theirsNum, :mine mineNum, :mineStrategy mineStrategy}))

; return a new round with an updated :mine based on the :mineStrategy and :theirs
(defn adjust-for-strategy [round]
  (let [new-mine (case (:mineStrategy round)
                   "lose" (mod (- (:theirs round) 1) 3)
                   "draw" (:theirs round)
                   "win" (mod (+ (:theirs round) 1) 3))]
    (assoc round :mine new-mine)))

; return a score incorporating the points from your hand and the result of the RPS round
(defn score-round [round]
  (+
   (+ 1 (:mine round))                                      ;; points for your hand
   (cond (= (:theirs round) (:mine round)) 3                ;; draw
         (= (mod (+ 1 (:theirs round)) 3) (:mine round)) 6  ;; win
         :else 0)))                                         ;; loss

; part 1
(println (->> input
              (map parse-line)
              (map score-round)
              (reduce +)))

; part 2
(println (->> input
              (map parse-line)
              (map adjust-for-strategy)
              (map score-round)
              (reduce +)))
