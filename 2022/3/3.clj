(require '[clojure.string :as str])
(require '[clojure.set :as set])

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

; as vector
(def input
  (vec (map str/trim-newline (str/split (slurp (first *command-line-args*)) #"\n"))))

; find duplicate letter across first and second halves of the line
(defn find-duplicate [line]
  (let [letters (str/split line #"")
        halves (split-at (quot (count letters) 2) letters)]
    (some (set (first halves)) (second halves))))

; find duplicate letter across 3 lines
(defn find-duplicate-across-lines [lines]
  (assert (= 3 (count lines)))
  (let [letters (map (fn [line] (str/split line #"")) lines)
        dupes (set/intersection (set (first letters)) (set (second letters)) (set (nth letters 2)))]
    (first dupes)))

(defn letter-priority [letter]
  (let [ascii-n (int (first letter))]
    (cond
      (<= 97 ascii-n 122) (- ascii-n 96)        ;; a-z 1-26
      (<= 65 ascii-n 90) (+ (- ascii-n 64) 26)  ;; A-Z 27-52
      :else (throw (Exception. "unexpected character")))))

; part 1
(println (->> input
              (map find-duplicate)
              (map letter-priority)
              (reduce +)))

; part 2
(println (->> input
              (partition 3)
              (map find-duplicate-across-lines)
              (map letter-priority)
              (reduce +)))
