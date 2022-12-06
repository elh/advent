(require '[clojure.string :as str])

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

(def inputs
  (vec
   (map #(Integer/parseInt %)
        (map clojure.string/trim-newline
             (str/split (slurp (first *command-line-args*)) #",")))))

; copied this from SO
(defn digits [n]
  (for [d  (str n)]
    (- (byte d) 48)))

(defn param-mode-code [op] 
  (let [ds (digits op)
        padded-ds (concat (for [_ (range (- 5 (count ds)))] 0) ds)]
    {
     :1stmode (nth padded-ds 2)
     :2ndmode (nth padded-ds 1)
     :3rdmode (nth padded-ds 0)
     :rawop (last padded-ds)
     }))

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
    (get program value); positional
    ))

(defn run
  ([program input] (run program input 0))
  ([program input pc]
   (let [f (frame program pc)
         op (first f)]
     (println "PROGRAM: " program "PC: " pc)
     (println "FRAME: " f)
     (if (= op 99) ; exit
       (do
         (println "exit!")
         program)
       (let [ pm-code (param-mode-code op)
             nextprogram-pc (case (get pm-code :rawop)
                              1 (do ; add
                              ;  (println "add: " "address: " (get f 3)
                              ;           "arg1: " (readv program (get f 1) (get pm-code :1stmode)) "arg2: " (readv program (get f 2) (get pm-code :2ndmode))
                              ;           "arg1_mode: " (get pm-code :1stmode) "arg2_mode: " (get pm-code :2ndmode))
                                  { :program (assoc program (get f 3)
                                                    (+
                                                     (readv program (get f 1) (get pm-code :1stmode))
                                                     (readv program (get f 2) (get pm-code :2ndmode))))
                                   :pc (+ pc (count f))}
                                  )
                              2 (do ; mult
                              ;  (println "mult: " "address: " (get f 3)
                              ;           "arg1: " (readv program (get f 1) (get pm-code :1stmode)) "arg2: " (readv program (get f 2) (get pm-code :2ndmode))
                              ;           "arg1_mode: " (get pm-code :1stmode) "arg2_mode: " (get pm-code :2ndmode))
                                  { :program (assoc program (get f 3) (*
                                                                       (readv program (get f 1) (get pm-code :1stmode))
                                                                       (readv program (get f 2) (get pm-code :2ndmode))))
                                   :pc (+ pc (count f))}
                                  )
                              3 (do ; input
                                  (println "input: ", input, "address: ", (get f 1))
                                  { :program (assoc program (get f 1) input)
                                   :pc (+ pc (count f))}
                                  )
                              4 (do ; output
                                  (println "output: " (readv program (get f 1) (get pm-code :1stmode)) "arg1_mode: " (get pm-code :1stmode))
                                  { :program program
                                   :pc (+ pc (count f))}
                                  )
                              5 (do ; jump if true
                                  { :program program
                                   :pc (if (not= 0 (readv program (get f 1) (get pm-code :1stmode)))
                                         (readv program (get f 2) (get pm-code :2ndmode))
                                         (+ pc (count f))
                                         )}
                                  )
                              6 (do ; jump if false
                                  {:program program
                                   :pc (if (= 0 (readv program (get f 1) (get pm-code :1stmode)))
                                         (readv program (get f 2) (get pm-code :2ndmode))
                                         (+ pc (count f)))})
                              7 (do ; less than
                                  {:program (assoc program (get f 3) (if (<
                                                                          (readv program (get f 1) (get pm-code :1stmode))
                                                                          (readv program (get f 2) (get pm-code :2ndmode)))
                                                                       1
                                                                       0))
                                   :pc (+ pc (count f))}
                                  )
                              8 (do ; equals
                                  { :program (assoc program (get f 3) (if (=
                                                                           (readv program (get f 1) (get pm-code :1stmode))
                                                                           (readv program (get f 2) (get pm-code :2ndmode)))
                                                                        1
                                                                        0))
                                   :pc (+ pc (count f))}
                                  )
                              (throw (Exception. (format "FAIL: unexpected opcode. got: %d" op))))]
         (run (get nextprogram-pc :program) input (get nextprogram-pc :pc)))))))

; pt 1
(println (time (run inputs 1)))

; pt 2
(println (time (run inputs 5)))
