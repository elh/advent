(defn digits [n]
  (for [d  (str n)]
    (- (byte d) 48)))

(defn validperms [a b]
  (filter (fn [perm] 
            (second
             (reduce (fn [data digit]
                       [digit (or (second data) (= (first data) digit))])
                     [-1 false]
                     perm)))
          (filter #(= (sort %) %)
                  (map digits
                       (range (+ a 1) b)))))

; p1
(println (time (count (validperms 108457 562041))))
; CORRECT: 2779

; p2
; (println (time (p2 inputs)))
; CORRECT: ???

