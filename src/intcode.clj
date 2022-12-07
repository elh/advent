(ns intcode
  (:require [clojure.string :as str]))

;; elh's Intcode machine
;; NOTE: Last updated for 2019 day 5

(def verbose false)

;; Opcodes
(def ADD 1)
(def MUL 2)
(def IN 3)
(def OUT 4)
(def JMPT 5)
(def JMPF 6)
(def LT 7)
(def EQ 8)
(def HALT 99)

(defn- digits [n]
  (for [d (str n)]
    (- (byte d) 48)))

(defn- param-mode-code
  "Parse instruction parameter mode and opcode."
  [op]
  (let [ds (digits op)
        padded-ds (concat (for [_ (range (- 5 (count ds)))] 0) ds)]
    {:1stmode (nth padded-ds 2)
     :2ndmode (nth padded-ds 1)
     :3rdmode (nth padded-ds 0)
     :rawop (last padded-ds)}))

(defn- frame
  "Read instruction frame at the program counter."
  [program pc]
  (let [l (condp = (:rawop (param-mode-code (nth program pc)))
            IN 2
            OUT 2
            JMPT 3
            JMPF 3
            HALT 1
            4)]
    (subvec program pc (min (+ pc l) (count program)))))

(defn- read-v
  "Read value respecting parameter mode: immediate or position."
  [program value mode]
  (if (= mode 1)
    value
    (get program value)))

(defn run
  "Run an Intcode program."
  ([program input] (run program input 0 []))
  ([program input pc outputs]
   (let [f (frame program pc)
         op (first f)]
     (if (= op HALT)
       (do
         (when verbose (println program))
         outputs)
       (let [pm-code (param-mode-code op)
             arg1 (read-v program (get f 1) (:1stmode pm-code))
             arg2 (read-v program (get f 2) (:2ndmode pm-code))
             next-pc (+ pc (count f)) ;; if not jumping
             next (condp = (:rawop pm-code)
                    ADD {:pc next-pc
                         :program (assoc program (get f 3) (+ arg1 arg2))}
                    MUL {:pc next-pc
                         :program (assoc program (get f 3) (* arg1 arg2))}
                    IN {:pc next-pc
                        :program (assoc program (get f 1) input)}
                    OUT {:pc next-pc
                         :program program}
                    JMPT {:pc (if (not= 0 arg1) arg2 next-pc)
                          :program program}
                    JMPF {:pc (if (zero? arg1) arg2 next-pc)
                          :program program}
                    LT {:pc next-pc
                        :program (assoc program (get f 3) (if (< arg1 arg2) 1 0))}
                    EQ {:pc next-pc
                        :program (assoc program (get f 3) (if (= arg1 arg2) 1 0))}
                    (throw (Exception. (format "FAIL: unexpected opcode. got: %d" op))))
             outputs (if (= (:rawop pm-code) OUT) (conj outputs arg1) outputs)]
         (run (:program next) input (:pc next) outputs))))))

(defn parse-program
  "Parse Intcode program from string to vector of integers."
  [s]
  (vec (map #(Integer/parseInt %) (map str/trim-newline (str/split s #",")))))
