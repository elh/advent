(ns day-5-clean
  (:require [clojure.string :as str]))

;; opcodes
(def ADD 1)
(def MUL 2)
(def IN 3)
(def OUT 4)
(def JMPT 5)
(def JMPF 6)
(def LT 7)
(def EQ 8)
(def HALT 99)

;; integer as vector of digits
(defn digits [n]
  (for [d (str n)]
    (- (byte d) 48)))

;; parse instruction parameter mode and opcode
(defn param-mode-code [op]
  (let [ds (digits op)
        padded-ds (concat (for [_ (range (- 5 (count ds)))] 0) ds)]
    {:1stmode (nth padded-ds 2)
     :2ndmode (nth padded-ds 1)
     :3rdmode (nth padded-ds 0)
     :rawop (last padded-ds)}))

;; instruction frame
(defn frame [program pc]
  (let [l (condp = (get (param-mode-code (nth program pc)) :rawop)
            IN 2
            OUT 2
            JMPT 3
            JMPF 3
            HALT 1
            4)]
    (subvec program pc (min (+ pc l) (count program)))))

;; read value respecting parameter mode: immediate or position
(defn readv [program value mode]
  (if (= mode 1)
    value
    (get program value)))

(defn run
  ([program input] (run program input 0 []))
  ([program input pc outputs]
   (let [f (frame program pc)
         op (first f)]
     (if (= op HALT)
       outputs
       (let [pm-code (param-mode-code op)
             nextprogram-pc (condp = (get pm-code :rawop)
                              ADD {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (+ (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))))}
                              MUL {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (* (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))))}
                              IN {:pc (+ pc (count f))
                                 :program (assoc program (get f 1) input)}
                              OUT {:pc (+ pc (count f))
                                 :program program}
                              JMPT {:pc (if (not= 0 (readv program (get f 1) (get pm-code :1stmode))) (readv program (get f 2) (get pm-code :2ndmode)) (+ pc (count f)))
                                 :program program}
                              JMPF {:pc (if (zero? (readv program (get f 1) (get pm-code :1stmode))) (readv program (get f 2) (get pm-code :2ndmode)) (+ pc (count f)))
                                 :program program}
                              LT {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (if (< (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))) 1 0))}
                              EQ {:pc (+ pc (count f))
                                 :program (assoc program (get f 3) (if (= (readv program (get f 1) (get pm-code :1stmode)) (readv program (get f 2) (get pm-code :2ndmode))) 1 0))}
                              (throw (Exception. (format "FAIL: unexpected opcode. got: %d" op))))
             outputs (if (= (get pm-code :rawop) 4) (conj outputs (readv program (get f 1) (get pm-code :1stmode))) outputs)]
         (run (get nextprogram-pc :program) input (get nextprogram-pc :pc) outputs))))))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [input (vec (map #(Integer/parseInt %) (map str/trim-newline (str/split (slurp (first args)) #","))))]
    (println "PART 1:" (time (run input 1)))
    (println "PART 2:" (time (run input 5)))))
