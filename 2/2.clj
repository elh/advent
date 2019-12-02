(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (do
    (println "expects exactly one command line arg. EXIT")
    (. System exit 1)))

(def inputs 
  (vec 
   (map #(Integer/parseInt %) 
        (map clojure.string/trim-newline
             (str/split (slurp (first *command-line-args*)) #",")))))

(defn frame [program pc]
  (subvec program (* pc 4) (min (* (+ pc 1) 4) (count program))))

(defn step 
  ([program] (step program 0))
  ([program pc] 
   (let [f (frame program pc)
         op (first f)]
     (if (= op 99) ; exit
       program
       (let [nextprogram (cond
                           (= op 1) (assoc program (get f 3) (+  (get program (get f 1)) (get program (get f 2)))) ; add
                           (= op 2) (assoc program (get f 3) (* (get program (get f 1)) (get program (get f 2)))) ; mult
                           :else (println "ERROR"))] 
         (step nextprogram (+ pc 1))
         )
     ))))

; Intcode computer
(def pt1 (assoc (assoc inputs 1 12) 2 2))
(println (time (step pt1)))

; CORRECT: 2692315
