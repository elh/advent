(ns intcode
  (:require [clojure.string :as str]))

;; elh's Intcode machine
;; NOTE: Last updated for 2019 day 5

(def verbose false)

(def ADD 1)
(def MUL 2)
(def IN 3)
(def OUT 4)
(def JMPT 5)
(def JMPF 6)
(def LT 7)
(def EQ 8)
(def HALT 99)

(defn- param-mode-opcode
  "Parse instruction parameter mode and opcode."
  [inst]
  (let [digits (for [d (str inst)] (- (byte d) 48))
        padded-ds (concat (for [_ (range (- 5 (count digits)))] 0) digits)]
    {:1st-mode (nth padded-ds 2)
     :2nd-mode (nth padded-ds 1)
     :3rd-mode (nth padded-ds 0)
     :op (last padded-ds)}))

(defn- frame
  "Read instruction frame at the program counter."
  [program pc]
  (let [l (condp = (:op (param-mode-opcode (nth program pc)))
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
  (if (= mode 1) value (get program value)))

(defn run
  "Run an Intcode program."
  ([program] (run program [] [] 0))
  ([program inputs] (run program inputs [] 0))
  ([program inputs outputs pc]
   (let [f (frame program pc)
         rawop (first f)
         pm-op (param-mode-opcode rawop)]
     (when verbose (println {:outputs outputs :program program}))
     (cond
       (= rawop HALT) {:outputs outputs :program program}
       (not-any? #(= (:op pm-op) %) [ADD MUL IN OUT JMPT JMPF LT EQ HALT]) (throw (Exception. (str "Invalid opcode: " (:op pm-op))))
       :else (let [arg1 (read-v program (get f 1) (:1st-mode pm-op))
                   arg2 (read-v program (get f 2) (:2nd-mode pm-op))
                   pc++ (condp = (:op pm-op)
                          JMPT (if (not= 0 arg1) arg2 (+ pc (count f)))
                          JMPF (if (zero? arg1) arg2 (+ pc (count f)))
                          (+ pc (count f)))
                   program++ (condp = (:op pm-op)
                               ADD (assoc program (get f 3) (+ arg1 arg2))
                               MUL (assoc program (get f 3) (* arg1 arg2))
                               IN (assoc program (get f 1) (first inputs))
                               LT (assoc program (get f 3) (if (< arg1 arg2) 1 0))
                               EQ (assoc program (get f 3) (if (= arg1 arg2) 1 0))
                               program)
                   inputs++ (if (= (:op pm-op) IN) (rest inputs) inputs)
                   outputs++ (if (= (:op pm-op) OUT) (conj outputs arg1) outputs)]
               (run program++ inputs++ outputs++ pc++))))))

(defn parse-program
  "Parse Intcode program from string to vector of integers."
  [s]
  (vec (map #(Integer/parseInt %) (map str/trim-newline (str/split (str/replace s #"\n" "") #",")))))
