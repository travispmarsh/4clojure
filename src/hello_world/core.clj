(ns hello-world.core)

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Hello World"})

((fn add-five [x] (+ x 5)) 3)
((fn [x] (+ x 5)) 3)
(#(+ % 5) 3)
((partial + 5) 3)

((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)

(nil? (:a {:a nil :b 2}))

((fn [x & y] (nil? (x y)) :a {:a nil :b 2}))

(partial nil? :a {:a nil :b 2})

(
  (fn [k m] (nil? (k m))) :b {:a nil :b 2})
(#(nil? (%1 %2)) :a {:a nil :b 2})

(#(nil? (%1 %2)) :b {:a nil :b 2})

;; :a {:a nil :b 2}

(fn [k m] (nil? (k m)))

((fn [k m] (and
             (nil? (k m))
             (contains? m k)) :b) {:a nil :b 2})

;; only nil and false represent the values of logical falsity in conditional tests - anything else is logical truth
;; all of these are 1
(if-not false 1 0)
(if-not nil 1 0)
(if true 1 0)
(if [] 1 0)
(if [0] 1 0)
(if 0 1 0)
(if 1 1 0)

(= 2 (:foo {:bar 0, :baz 1} 2))

#(d s)

(for [x [ 1 2 3 4]
      :let [result (conj x result)]]
  result)

(for [x [0 1 2 3 4 5]
      :let [y (* x 3)]
      :when (even? y)]
  y)

(#(reduce conj () %) [1 2 3])

#(= %1 (if (string? %1)
         (apply str (reverse %1))
         (reverse %1)))

(#(= %1 (reverse %1)) [:foo :bar :foo])

(f)

 #(loop [result [1]
           x 0
           y 1
           len (- %1 1)]
      (if (zero? len)
        result
        (recur (conj result (+ x y)) y (+ x y) (dec len)))) 6

(#(last (sort (list %1 & %2))) 1 8 3 4)

#(loop [result [%1]
           cur %1]
      (if (= %2 (+ (last result) 1))
        result
        (recur (conj result (+ cur 1)) (+ cur 1)))) 1 4

(#(loop [result (first %)
         cur 1]
    (if (>= cur (count %))
      result
      (if (not= (nth % cur) (nth % (inc cur)))
        (recur (conj result (nth cur %)) (inc cur))
        (recur result (inc cur))
        ))
    ) [1 1 1 2 2 3])

 #(loop [result 1
           cur %1]
      (if (= cur 1)
        result
        (recur (* result cur) (dec cur))))

#(reduce * (range 1 (+ % 1))) 3

(last (sort (list (conj x more))))

((fn [s]
    (apply str (filter #(Character/isUpperCase %) s))) "nothingd")

(just-Upper "HeLlO, WoRlD!")

(def myquestions [{:question "how do you do?" :groups ["Intro" "Pleasantry"]}
                  {:question "howdie?" :groups ["Intro" "Pleasantry"]}]
  )

(map :groups myquestions)

()
(fn [x]
  (interleave x x)) [1 2 3]

(into () (mapcat [1 2] [:a :b]))

(#(mapcat list %1 %2) [1 2 3] [:a :b :c])

 (zipmap [1 2] [:a :b])

((fn my-flatten [seq]
   (if (coll? seq)
     (mapcat my-flatten seq)
     [seq])) '((1 2) 3 [4 [5 6]]))