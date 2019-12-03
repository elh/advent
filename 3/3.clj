(require '[clojure.string :as str])
(require 'clojure.set)

(when (not= (count *command-line-args*) 1)
  (throw (Exception. (format "FAIL: expects 1 cmdline arg. got: %d" (count *command-line-args*)))))

; ([R8 U5 L5 D3] [U7 R6 D4 L4])
(def inputs 
  (map (fn [x] (str/split x #","))
       (clojure.string/split-lines (slurp (first *command-line-args*)))))

(defn walk [start dir mag]
  (for [i (range 1 (+ mag 1))]
    (cond
      (= dir "U") [(first start) (+ (second start) i)]
      (= dir "L") [(- (first start) i) (second start)]
      (= dir "R") [(+ (first start) i) (second start)]
      (= dir "D") [(first start) (- (second start) i)]
      :else (throw (Exception. (format "FAIL: unexpected direction. got: %d" dir))))))

; outputs [end points] idx 0 is a vector of x y. idx 1 is set of points
(defn makepoints [linevec]
  ; data is [start points]. idx 0 is a vector of x y. idx 1 is set of points
  (reduce (fn [data segment] 
            (let [dir (subs segment 0 1)
                  mag (read-string (subs segment 1 (count segment)))
                  segmentps (walk (first data) dir mag)]
              (reduce (fn [d p] [p (conj (second d) p)])
                      data
                      segmentps)))
          [[0 0] (hash-set)]
          linevec))

; NOTE: I solved pt 2 additively since I am still not familiar with clojure structs so perf is horrific
; TODO: easy. can still do additively with lower perf by only generating wire delay once
(defn wiredelay [target linevec]
  (let [pts (reduce (fn [data segment]
                      (let [dir (subs segment 0 1)
                            mag (read-string (subs segment 1 (count segment)))
                            segmentps (walk (first data) dir mag)]
                        (reduce (fn [d p] [p (conj (second d) p)])
                                data
                                segmentps)))
                    [[0 0] [[0 0]]]
                    linevec)]
    (.indexOf (second pts) target)))

(defn manhattan [point]
  (+ (Math/abs (first point)) (Math/abs (second point))))

(defn closestintersect [points1 points2] 
  (first (sort (fn [a b] 
                 (cond (> (manhattan a) (manhattan b)) 1
                       (< (manhattan a) (manhattan b)) -1
                       :else 0)) 
               (clojure.set/intersection points1 points2))))

(defn p1 [inputs]
  (manhattan 
   (closestintersect 
    (second (makepoints (first inputs))) 
    (second (makepoints (second inputs))))))

(defn p2 [inputs]
  (let [wire1 (first inputs)
        wire2 (second inputs)
        intersections (clojure.set/intersection
                       (second (makepoints wire1))
                       (second (makepoints wire2)))]
    (reduce (fn [mindelay intersect]
              (min mindelay (+ (wiredelay intersect wire1) (wiredelay intersect wire2))))
            Integer/MAX_VALUE
            intersections)))

; p1
(println (time (p1 inputs)))
; CORRECT: 651

; p2
(println (time (p2 inputs)))
; CORRECT: 7534
