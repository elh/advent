(ns day-5-clean
  (:require [clojure.string :as str]))

; copied this from SO
(defn digits [n]
  (for [d (str n)]
    (- (byte d) 48)))

(defn param-mode-code [op]
  (let [ds (digits op)
        padded-ds (concat (for [_ (range (- 5 (count ds)))] 0) ds)]
    {:1stmode (nth padded-ds 2)
     :2ndmode (nth padded-ds 1)
     :3rdmode (nth padded-ds 0)
     :rawop (last padded-ds)}))

(defn frame [program pc]
  (let [l (case (get (param-mode-code (nth program pc)) :rawop)
            3 2
            4 2
            5 3
            6 3
            99 1
            4)]
    (subvec program pc (min (+ pc l) (count program)))))

(defn readv [program value mode]
  (if (= mode 1)
    value ; immediate
    (get program value))); positional

(defn run
  ([program input] (run program input 0))
  ([program input pc]
   (let [f (frame program pc)
         op (first f)]
     (if (= op 99) ; exit
       (do
         (println "exit!")
         program)
       (let [pm-code (param-mode-code op)
             nextprogram-pc (case (get pm-code :rawop)
                              ; add
                              1 {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (+ (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))))}
                              ; mult
                              2 {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (* (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))))}
                              ; input
                              3 (do
                                  (println "input: ", input, "address: ", (get f 1))
                                  {:pc (+ pc (count f))
                                   :program (assoc program (get f 1) input)})
                              ; output
                              4 (do
                                  (println "output: " (readv program (get f 1) (get pm-code :1stmode)) "arg1_mode: " (get pm-code :1stmode))
                                  {:pc (+ pc (count f))
                                   :program program})
                              ; jump if true
                              5 {:pc (if (not= 0 (readv program (get f 1) (get pm-code :1stmode))) (readv program (get f 2) (get pm-code :2ndmode)) (+ pc (count f)))
                                 :program program}
                              ; jump if false
                              6 {:pc (if (zero? (readv program (get f 1) (get pm-code :1stmode))) (readv program (get f 2) (get pm-code :2ndmode)) (+ pc (count f)))
                                 :program program}
                              ; less than
                              7 {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (if (< (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))) 1 0))}
                              ; equals
                              8 {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (if (= (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))) 1 0))}
                              (throw (Exception. (format "FAIL: unexpected opcode. got: %d" op))))]
         (run (get nextprogram-pc :program) input (get nextprogram-pc :pc)))))))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [input (vec (map #(Integer/parseInt %) (map str/trim-newline (str/split (slurp (first args)) #","))))]
    (println "part 1:" (time (run input 1)))
    (println "part 2:" (time (run input 5)))))
