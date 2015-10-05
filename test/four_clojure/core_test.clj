(ns four-clojure.core-test
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [four-clojure.core :refer :all]))


;; ;; "/​problem/​1"
;; (deftest test-nothing-but-the-truth
;;   (is (= true true)))

;; ;; "/​problem/​2"
;; (deftest test-simple-math
;;   (is (= (- 10 (* 2 3)) 4)))

;; ;; "/​problem/​3"
;; (deftest test-intro-to-strings
;;   (is (= (.toUpperCase "hello world") "HELLO WORLD")))

;; ;; "/​problem/​4"
;; (deftest test-intro-to-lists
;;   (is (= '(:a :b :c) (list :a :b :c))))

;; ;; "/​problem/​5"
;; (deftest test-lists-conj
;;   (is (= [1 2 3 4] (conj '(2 3 4) 1)))
;;   (is (= [1 2 3 4] (conj '(3 4) 2 1))))

;; ;; "/​problem/​6"
;; (deftest test-intro-to-vectors
;;   (is (= [:a :b :c] (list :a :b :c)))
;;   (is (= [:a :b :c] (vec '(:a :b :c))))
;;   (is (= [:a :b :c] (vector :a :b :c))))

;; ;; "/​problem/​7"
;; (deftest test-vectors-conj
;;   (is (= [1 2 3 4] (conj [1 2 3] 4)))
;;   (is (= [1 2 3 4] (conj [1 2] 3 4))))

;; ;; "/​problem/​8"
;; (deftest test-intro-to-sets
;;   (is (= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d))))
;;   (is (= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))))

;; ;; "/​problem/​9"
;; (deftest test-sets-conj
;;   (is (= #{1 2 3 4} (conj #{1 4 3} 2))))

;; ;; "/​problem/​10"
;; (deftest test-intro-to-maps
;;   (is (= 20 (:b {:a 10, :b 20, :c 30})))
;;   (is (= 20 ((hash-map :a 10, :b 20, :c 30) :b))))

;; ;; "/​problem/​11"
;; (deftest test-maps-conj
;;   (is (= {:a 1, :b 2, :c 3} (conj {:a 1} maps-conj [:c 3]))))

;; ;; "/​problem/​12"
;; (deftest test-intro-to-sequences
;;   (is (= 3 (first '(3 2 1))))
;;   (is (= 3 (second [2 3 4])))
;;   (is (= 3 (last (list 1 2 3)))))

;; ;; "/​problem/​13"
;; (deftest test-sequences-rest
;;   (= [20 30 40] (rest [10 20 30 40])))

;; ;; "/​problem/​14"
;; (deftest test-intro-to-functions
;;   (is (= 8 ((fn add-five [x] (+ x 5)) 3)))
;;   (is (= 8 ((fn [x] (+ x 5)) 3)))
;;   (is (= 8 (#(+ % 5) 3)))
;;   (is (= 8 ((partial + 5) 3))))

;; ;; "/​problem/​15"
;; (deftest test-double-down
;;   (is (= (double-down 2) 4))
;;   (is (= (double-down 3) 6))
;;   (is (= (double-down 11) 22))
;;   (is (= (double-down 7) 14)))

;; ;; "/​problem/​16"
;; (deftest test-hello-world
;;   (is (= (hello-world "Dave") "Hello, Dave!"))
;;   (is (= (hello-world "Jenn") "Hello, Jenn!"))
;;   (is (= (hello-world "Rhea") "Hello, Rhea!")))

;; ;; "/​problem/​17"
;; (deftest test-sequences-map
;;   (is (= [6 7 8] (map #(+ % 5) '(1 2 3)))))

;; ;; "/​problem/​18"
;; (deftest test-sequences-filter
;;   (is (= [6 7] (filter #(> % 5) '(3 4 5 6 7)))))

;; ;; "/​problem/​19"
;; (deftest test-last-element
;;   (is (= (last-element [1 2 3 4 5]) 5))
;;   (is (= (last-element '(5 4 3)) 3))
;;   (is (= (last-element ["b" "c" "d"]) "d")))

;; ;; "/​problem/​20"
;; (deftest test-penultimate-element
;;   (is (= (penultimate-element (list 1 2 3 4 5)) 4))
;;   (is (= (penultimate-element ["a" "b" "c"]) "b"))
;;   (is (= (penultimate-element [[1 2] [3 4]]) [1 2])))

;; ;; "/​problem/​21"
;; (deftest test-nth-element
;;   (is (= (nth-element '(4 5 6 7) 2) 6))
;;   (is (= (nth-element [:a :b :c] 0) :a))
;;   (is (= (nth-element [1 2 3 4] 1) 2))
;;   (is (= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6])))

;; ;; "/​problem/​22"
;; (deftest test-count-a-sequence
;;   (is (= (count-a-sequence '(1 2 3 3 1)) 5))
;;   (is (= (count-a-sequence "Hello World") 11))
;;   (is (= (count-a-sequence [[1 2] [3 4] [5 6]]) 3))
;;   (is (= (count-a-sequence '(13)) 1))
;;   (is (= (count-a-sequence '(:a :b :c)) 3)))

;; ;; "/​problem/​23"
;; (deftest test-reverse-a-sequence
;;   (is (= (reverse-a-sequence [1 2 3 4 5]) [5 4 3 2 1]))
;;   (is (= (reverse-a-sequence (sorted-set 5 7 2 7)) '(7 5 2)))
;;   (is (= (reverse-a-sequence [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;; ;; "/​problem/​24"
;; (deftest test-sum-it-all-up
;;   (is (= (sum-it-all-up [1 2 3]) 6))
;;   (is (= (sum-it-all-up (list 0 -2 5 5)) 8))
;;   (is (= (sum-it-all-up #{4 2 1}) 7))
;;   (is (= (sum-it-all-up '(0 0 -1)) -1))
;;   (is (= (sum-it-all-up '(1 10 3)) 14)))

;; ;; "/​problem/​25"
;; (deftest test-find-the-odd-numbers
;;   (is (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5)))
;;   (is (= (find-the-odd-numbers [4 2 1 6]) '(1)))
;;   (is (= (find-the-odd-numbers [2 2 4 6]) '()))
;;   (is (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3))))

;; ;; "/​problem/​26"
;; (deftest test-fibonacci-sequence
;;   (is (= (fibonacci-sequence 3) '(1 1 2)))
;;   (is (= (fibonacci-sequence 6) '(1 1 2 3 5 8)))
;;   (is (= (fibonacci-sequence 8) '(1 1 2 3 5 8 13 21))))

;; ;; "/​problem/​27"
;; (deftest test-palindrome-detector
;;   (is (false? (palindrome-detector '(1 2 3 4 5))))
;;   (is (true? (palindrome-detector "racecar")))
;;   (is (true? (palindrome-detector [:foo :bar :foo])))
;;   (is (true? (palindrome-detector '(1 1 3 3 1 1))))
;;   (is (false? (palindrome-detector '(:a :b :c)))))

;; ;; "/​problem/​28"
;; (deftest test-flatten-a-sequence
;;   (is (= (flatten-a-sequence '(1 2 3)) '(1 2 3)))
;;   (is (= (flatten-a-sequence '((1))) '(1)))
;;   (is (= (flatten-a-sequence '((1 2 3))) '(1 2 3)))

;;   (is (= (flatten-a-sequence '((1) (2 3))) '(1 2 3)))
;;   (is (= (flatten-a-sequence '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
;;   (is (= (flatten-a-sequence ["a" ["b"] "c"]) '("a" "b" "c")))
;;   (is (= (flatten-a-sequence '((((:a))))) '(:a))))

;; ;; "/​problem/​29"
;; (deftest test-get-the-caps
;;   (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD"))
;;   (is (empty? (get-the-caps "nothing")))
;;   (is (= (get-the-caps "$#A(*&987Zf") "AZ")))

;; ;; "/​problem/​30"
;; (deftest test-compress-a-sequence
;;   (is (= (apply str (compress-a-sequence "Leeeeeerrroyyy")) "Leroy"))
;;   (is (= (compress-a-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
;;   (is (= (compress-a-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

;; ;; "/​problem/​31"
;; (deftest test-pack-a-sequence
;;   (is (= (pack-a-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
;;   (is (= (pack-a-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
;;   (is (= (pack-a-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

;; ;; "/​problem/​32"
;; (deftest test-duplicate-a-sequence
;;   (is (= (duplicate-a-sequence [1 2 3]) '(1 1 2 2 3 3)))
;;   (is (= (duplicate-a-sequence [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
;;   (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
;;   (is (= (duplicate-a-sequence [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

;; ;; "/​problem/​33"
;; (deftest test-replicate-a-sequence
;;   (is (= (replicate-a-sequence [1 2 3] 2) '(1 1 2 2 3 3)))
;;   (is (= (replicate-a-sequence [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
;;   (is (= (replicate-a-sequence [4 5 6] 1) '(4 5 6)))
;;   (is (= (replicate-a-sequence [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
;;   (is (= (replicate-a-sequence [44 33] 2) [44 44 33 33])))

;; ;; "/​problem/​34"
;; (deftest test-implement-range
;;   (is (= (implement-range 1 4) '(1 2 3)))
;;   (is (= (implement-range -2 2) '(-2 -1 0 1)))
;;   (is (= (implement-range 5 8) '(5 6 7)))
;;   (is (= (implement-range 5 8) '(5 6 7))))

;; ;; "/​problem/​35"
;; (deftest test-local-bindings
;;   (is (= 7 (let [x 5] (+ 2 x))))
;;   (is (= 7 (let [x 3, y 10] (- y x))))
;;   (is (= 7 (let [x 21] (let [y 3] (/ x y))))))

;; ;; "/​problem/​36"
;; (deftest test-let-it-be
;;   (is (= 10 (let [z 1, y 3, x 7] (+ x y))))
;;   (is (= 4 (let [z 1, y 3, x 7] (+ y z))))
;;   (is (= 1 (let [z 1, y 3, x 7] z)))
;;   (is (= 1 (let [z 1, y 3, x 7] z))))

;; ;; "/​problem/​37"
;; (deftest test-regular-expressions
;;   (is (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))))

;; ;; "/​problem/​38"
;; (deftest test-maximum-value
;;   (is (= (maximum-value 1 8 3 4) 8))
;;   (is (= (maximum-value 30 20) 30))
;;   (is (= (maximum-value 45 67 11) 67)))

;; ;; "/​problem/​39"
;; (deftest test-interleave-two-seqs
;;   (is (= (interleave-two-seqs [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
;;   (is (= (interleave-two-seqs [1 2] [3 4 5 6]) '(1 3 2 4)))
;;   (is (= (interleave-two-seqs [1 2 3 4] [5]) [1 5]))
;;   (is (= (interleave-two-seqs [30 20] [25 15]) [30 25 20 15])))

;; ;; "/​problem/​40"
;; (deftest test-interpose-a-seq
;;   (is (= (interpose-a-seq 0 [1 2 3]) [1 0 2 0 3]))
;;   (is (= (apply str (interpose-a-seq ", " ["one" "two" "three"])) "one, two, three"))
;;   (is (= (interpose-a-seq :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
;;   (is (= (interpose-a-seq :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

;; ;; "/​problem/​41"
;; (deftest test-drop-every-nth-item
;;   (is (= (drop-every-nth-item [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
;;   (is (= (drop-every-nth-item [:a :b :c :d :e :f] 2) [:a :c :e]))
;;   (is (= (drop-every-nth-item [1 2 3 4 5 6] 4) [1 2 3 5 6]))
;;   (is (= (drop-every-nth-item [1 2 3 4 5 6] 4) [1 2 3 5 6])))

;; ;; "/​problem/​42"
;; (deftest test-factorial-fun
;;   (is (= (factorial-fun 1) 1))
;;   (is (= (factorial-fun 3) 6))
;;   (is (= (factorial-fun 5) 120))
;;   (is (= (factorial-fun 8) 40320)))

;; ;; "/​problem/​43"
;; (deftest test-reverse-interleave
;;   (is (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
;;   (is (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
;;   (is (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

;; ;; "/​problem/​44"
;; (deftest test-rotate-sequence
;;   (is (= (rotate-sequence 2 [1 2 3 4 5]) '(3 4 5 1 2)))
;;   (is (= (rotate-sequence -2 [1 2 3 4 5]) '(4 5 1 2 3)))
;;   (is (= (rotate-sequence 6 [1 2 3 4 5]) '(2 3 4 5 1)))
;;   (is (= (rotate-sequence 1 '(:a :b :c)) '(:b :c :a)))
;;   (is (= (rotate-sequence -4 '(:a :b :c)) '(:c :a :b))))

;; ;; "/​problem/​45"
;; (deftest test-intro-to-iterate
;;   (is (= [1 4 7 10 13] (take 5 (iterate #(+ 3 %) 1)))))

;; ;; "/​problem/​46"
;; (deftest test-flipping-out
;;   (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
;;   (is (= true ((flipping-out >) 7 8)))
;;   (is (= 4 ((flipping-out quot) 2 8)))
;;   (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

;; ;; "/​problem/​47"
;; (deftest test-contain-yourself
;;   (let [ans 4]
;;     (is (contains? #{4 5 6} ans))
;;     (is (contains? [1 1 1 1 1] ans))
;;     (is (contains? {4 :a 2 :b} ans))
;;     (is (not (contains? [1 2 4] ans)))))

;; ;; "/​problem/​48"
;; (deftest test-intro-to-some
;;   (let [ans 6]
;;     (is (= ans (some #{2 7 6} [5 6 7 8])))
;;     (is (= ans (some #(when (even? %) %) [5 6 7 8])))))

;; ;; "/​problem/​49"
;; (deftest test-split-a-sequence
;;   (is (= (split-a-sequence 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
;;   (is (= (split-a-sequence 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
;;   (is (= (split-a-sequence 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

;; ;; "/​problem/​50"
;; (deftest test-split-by-type
;;   (is (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
;;   (is (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
;;   (is (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

;; ;; "/​problem/​51"
;; (deftest test-advanced-destructuring
;;   (let [ans (range 1 6)]
;;     (is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] ans] [a b c d])))))

;; ;; "/​problem/​52"
;; (deftest test-intro-to-destructuring
;;   (let [ans '[c e]]
;;     (= [2 4] (let [[a b c d e f g] (range)]
;;                ans))))

;; ;; "/​problem/​53"
;; (deftest test-longest-increasing-sub-seq
;;   (is (= (longest-increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3]))
;;   (is (= (longest-increasing-sub-seq [5 6 1 3 2 7]) [5 6]))
;;   (is (= (longest-increasing-sub-seq [2 3 3 4 5]) [3 4 5]))
;;   (is (= (longest-increasing-sub-seq [7 6 5 4]) [])))

;; ;; "/​problem/​54"
;; (deftest test-partition-a-sequence
;;   (is (= (partition-a-sequence 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
;;   (is (= (partition-a-sequence 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
;;   (is (= (partition-a-sequence 3 (range 8)) '((0 1 2) (3 4 5))))
;;   (is (= (partition-a-sequence 3 (range 8)) '((0 1 2) (3 4 5)))))

;; ;; "/​problem/​55"
;; (deftest test-count-occurrences
;;   (is (= (count-occurrences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
;;   (is (= (count-occurrences [:b :a :b :a :b]) {:a 2, :b 3}))
;;   (is (= (count-occurrences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

;; ;; "/​problem/​56"
;; (deftest test-find-distinct-items
;;   (is (= (find-distinct-items [1 2 1 3 1 2 4]) [1 2 3 4]))
;;   (is (= (find-distinct-items [:a :a :b :b :c :c]) [:a :b :c]))
;;   (is (= (find-distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
;;   (is (= (find-distinct-items (range 50)) (range 50))))

;; ;; "/​problem/​57"
;; (deftest test-simple-recursion
;;   (is (= [5 4 3 2 1] ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))))

;; ;; "/​problem/​58"
;; (deftest test-function-composition
;;   (is (= [3 2 1] ((function-composition rest reverse) [1 2 3 4])))
;;   (is (= 5 ((function-composition (partial + 3) second) [1 2 3 4])))
;;   (is (= true ((function-composition zero? #(mod % 8) +) 3 5 7 9)))
;;   (is (= "HELLO" ((function-composition #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;; "/​problem/​59"
(deftest test-juxtaposition
  (is (= [21 6 1] ((juxtaposition + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((juxtaposition #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((juxtaposition :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

;; "/​problem/​60"
(deftest test-sequence-reductions
  (is (= (take 5 (sequence-reductions + (range))) [0 1 3 6 10]))
  (is (= (sequence-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (sequence-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

;; "/​problem/​61"
(deftest test-map-construction
  (is (= (map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))
  (is (= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

;; "/​problem/​62"
(deftest test-reimplement-iterate
  (is (= (take 5 (reimplement-iterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (reimplement-iterate inc 0)) (take 100 (range))))
  (is (= (take 9 (reimplement-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

;; "/​problem/​63"
(deftest test-group-a-sequence
  (is (= (group-a-sequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-a-sequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-a-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

;; "/​problem/​64"
(deftest test-intro-to-reduce
  (is (= 15 (reduce intro-to-reduce [1 2 3 4 5])))
  (is (=  0 (reduce intro-to-reduce [])))
  (is (=  6 (reduce intro-to-reduce 1 [2 3])))
  (is (=  6 (reduce intro-to-reduce 1 [2 3]))))

;; "/​problem/​65"
#_(deftest test-black-box-testing
    (is (= :map (black-box-testing {:a 1, :b 2})))
    (is (= :list (black-box-testing (range (rand-int 20)))))
    (is (= :vector (black-box-testing [1 2 3 4 5 6])))
    (is (= :set (black-box-testing #{10 (rand-int 5)})))
    (is (= [:map :set :vector :list] (map black-box-testing [{} #{} [] ()]))))

;; "/​problem/​66"
(deftest test-greatest-common-divisor
  (is (= (greatest-common-divisor 2 4) 2))
  (is (= (greatest-common-divisor 10 5) 5))
  (is (= (greatest-common-divisor 5 7) 1))
  (is (= (greatest-common-divisor 1023 858) 33)))

;; "/​problem/​67"
(deftest test-prime-numbers
  (is (= (prime-numbers 2) [2 3]))
  (is (= (prime-numbers 5) [2 3 5 7 11]))
  (is (= (last (prime-numbers 100)) 541)))

;; "/​problem/​68"
(deftest test-recurring-theme
  (= [7 6 5 4 3] (loop [x 5
                        result []]
                   (if (> x 0)
                     (recur (dec x) (conj result (+ 2 x)))
                     result))))

;; "/​problem/​69"
(deftest test-merge-with-a-function
  (is (= (merge-with-a-function * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}) {:a 4, :b 6, :c 20}))
  (is (= (merge-with-a-function - {1 10, 2 20} {1 3, 2 10, 3 15}) {1 7, 2 10, 3 15}))
  (is (= (merge-with-a-function concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}) {:a [3 4 5], :b [6 7], :c [8 9]})))

;; "/​problem/​70"
(deftest test-word-sorting
  (is (= (word-sorting  "Have a nice day.") ["a" "day" "Have" "nice"]))
  (is (= (word-sorting  "Clojure is a fun language!") ["a" "Clojure" "fun" "is" "language"]))
  (is (= (word-sorting  "Fools fall for foolish follies.") ["fall" "follies" "foolish" "Fools" "for"])))

;; "/​problem/​71"
(deftest test-rearranging-code-->
  (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
     5))

;; "/​problem/​72"
(deftest test-rearranging-code-->>
  (= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
     (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +))
     11))

;; "/​problem/​73"
(deftest test-analyze-a-tic-tac-toe-board
  (is (= nil (analyze-a-tic-tac-toe-board [[:e :e :e]
                                           [:e :e :e]
                                           [:e :e :e]])))
  (is (= :x (analyze-a-tic-tac-toe-board [[:x :e :o]
                                          [:x :e :e]
                                          [:x :e :o]])))
  (is (= :o (analyze-a-tic-tac-toe-board [[:e :x :e]
                                          [:o :o :o]
                                          [:x :e :x]])))
  (is (= nil (analyze-a-tic-tac-toe-board [[:x :e :o]
                                           [:x :x :e]
                                           [:o :x :o]])))
  (is (= :x (analyze-a-tic-tac-toe-board [[:x :e :e]
                                          [:o :x :e]
                                          [:o :e :x]])))
  (is (= :o (analyze-a-tic-tac-toe-board [[:x :e :o]
                                          [:x :o :e]
                                          [:o :e :x]])))
  (is (= nil (analyze-a-tic-tac-toe-board [[:x :o :x]
                                           [:x :o :x]
                                           [:o :x :o]]))))

;; "/​problem/​74"
(deftest test-filter-perfect-squares
  (is (= (filter-perfect-squares "4,5,6,7,8,9") "4,9"))
  (is (= (filter-perfect-squares "15,16,25,36,37") "16,25,36")))

;; "/​problem/​75"
(deftest test-eulers-totient-function
  (is (= (eulers-totient-function 1) 1))
  (is (= (eulers-totient-function 10) (count '(1 3 7 9)) 4))
  (is (= (eulers-totient-function 40) 16))
  (is (= (eulers-totient-function 99) 60))
  (is (= (eulers-totient-function 99) 60)))

;; "/​problem/​76"
(deftest test-intro-to-trampoline
  (= [1 3 5 7 9 11]
     (letfn [(foo [x y] #(bar (conj x y) y))
             (bar [x y] (if (> (last x) 10)
                          x
                          #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))

;; "/​problem/​77"
(deftest test-anagram-finder
  (is (= (anagram-finder ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}}))
  (is (= (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"]) #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

;; "/​problem/​78"
(deftest test-reimplement-trampoline
  (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                 (sub-two [x] #(stop?(- x 2)))
                 (stop? [x] (if (> x 50) x #(triple x)))]
           (reimplement-trampoline triple 2))
         82))
  (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                 (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
           (map (partial reimplement-trampoline my-even?) (range 6)))
         [true false true false true false])))

;; "/​problem/​79"
(deftest test-triangle-minimal-path
  (is (= 3 (triangle-minimal-path [[3]])))
  (is (= 3 (triangle-minimal-path [[1]
                                   [2 4]])))
  (is (= 7 (triangle-minimal-path '([1]
                                    [2 4]
                                    [5 1 4]
                                    [2 3 4 5]))))              ; 1->2->1->3
  (is (= 20 (triangle-minimal-path '([3]
                                     [2 4]
                                     [1 9 3]
                                     [9 9 2 4]
                                     [4 6 6 7 8]
                                     [5 7 3 5 1 4]))))         ; 3->4->3->2->7->1
  )

;; "/​problem/​80"
(deftest test-perfect-numbers
  (is (= (perfect-numbers 6) true))
  (is (= (perfect-numbers 7) false))
  (is (= (perfect-numbers 496) true))
  (is (= (perfect-numbers 500) false))
  (is (= (perfect-numbers 8128) true)))

;; "/​problem/​81"
(deftest test-set-intersection
  (is (= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (set-intersection #{0 1 2} #{3 4 5}) #{}))
  (is (= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

;; "/​problem/​82"
#_(deftest test-word-chains
  (testing "word-differences"
    (are [expected lhs rhs] (= expected (word-difference lhs rhs))
      1 "cat" "cot"
      1 "cot" "coat"
      1 "coat" "oat"
      0 "oat" "oat"))

  (testing "word-chains"
    (is (= true (word-chains ["cat" "cot" "coat" "oat" "hat" "hot" "hog" "dog"])))
    (is (= true (word-chains #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
    (is (= false (word-chains #{"cot" "hot" "bat" "fat"})))
    (is (= false (word-chains #{"to" "top" "stop" "tops" "toss"})))
    (is (= true (word-chains #{"spout" "do" "pot" "pout" "spot" "dot"})))
    (is (= true (word-chains #{"share" "hares" "shares" "hare" "are"})))
    (is (= false (word-chains #{"share" "hares" "hare" "are"})))))

;; "/​problem/​83"
(deftest test-a-half-truth
  (is (= false (a-half-truth false false)))
  (is (= true (a-half-truth true false)))
  (is (= false (a-half-truth true)))
  (is (= true (a-half-truth false true false)))
  (is (= false (a-half-truth true true true)))
  (is (= true (a-half-truth true true true false))))

;; "/​problem/​84"
(deftest test-transitive-closure
  )

;; "/​problem/​85"
(deftest test-power-set
  )

;; "/​problem/​86"
(deftest test-happy-numbers
  )

;; "/​problem/​88"
(deftest test-symmetric-difference
  )

;; "/​problem/​89"
(deftest test-graph-tour
  )

;; "/​problem/​90"
(deftest test-cartesian-product
  )

;; "/​problem/​91"
(deftest test-graph-connectivity
  )

;; "/​problem/​92"
(deftest test-read-roman-numerals
  )

;; "/​problem/​93"
(deftest test-partially-flatten-a-sequence
  )

;; "/​problem/​94"
(deftest test-game-of-life
  )

;; "/​problem/​95"
(deftest test-to-tree-or-not-to-tree
  )

;; "/​problem/​96"
(deftest test-beauty-is-symmetry
  )

;; "/​problem/​97"
(deftest test-pascals-triangle
  )

;; "/​problem/​98"
(deftest test-equivalence-classes
  )

;; "/​problem/​99"
(deftest test-product-digits
  )

;; "/​problem/​100"
(deftest test-least-common-multiple
  )

;; "/​problem/​101"
(deftest test-levenshtein-distance
  )

;; "/​problem/​102"
(deftest test-intocamelcase
  )

;; "/​problem/​103"
(deftest test-generating-k-combinations
  )

;; "/​problem/​104"
(deftest test-write-roman-numerals
  )

;; "/​problem/​105"
(deftest test-identify-keys-and-values
  )

;; "/​problem/​106"
(deftest test-number-maze
  )

;; "/​problem/​107"
(deftest test-simple-closures
  )

;; "/​problem/​108"
(deftest test-lazy-searching
  )

;; "/​problem/​110"
(deftest test-sequence-of-pronunciations
  )

;; "/​problem/​111"
(deftest test-crossword-puzzle
  )

;; "/​problem/​112"
(deftest test-sequs-horribilis
  )

;; "/​problem/​113"
(deftest test-making-data-dance
  )

;; "/​problem/​114"
(deftest test-global-take-while
  )

;; "/​problem/​115"
(deftest test-the-balance-of-n
  )

;; "/​problem/​116"
(deftest test-prime-sandwich
  )

;; "/​problem/​117"
(deftest test-for-science!
  )

;; "/​problem/​118"
(deftest test-re-implement-map
  )

;; "/​problem/​119"
(deftest test-win-at-tic-tac-toe
  )

;; "/​problem/​120"
(deftest test-sum-of-square-of-digits
  )

;; "/​problem/​121"
(deftest test-universal-computation-engine
  )

;; "/​problem/​122"
(deftest test-read-a-binary-number
  )

;; "/​problem/​124"
(deftest test-analyze-reversi
  )

;; "/​problem/​125"
(deftest test-gus-quinundrum
  )

;; "/​problem/​126"
(deftest test-through-the-looking-class
  )

;; "/​problem/​127"
(deftest test-love-triangle
  )

;; "/​problem/​128"
(deftest test-recognize-playing-cards
  )

;; "/​problem/​130"
(deftest test-tree-reparenting
  )

;; "/​problem/​131"
(deftest test-sum-some-set-subsets
  )

;; "/​problem/​132"
(deftest test-insert-between-two-items
  )

;; "/​problem/​134"
(deftest test-a-nil-key
  )

;; "/​problem/​135"
(deftest test-infix-calculator
  )

;; "/​problem/​137"
(deftest test-digits-and-bases
  )

;; "/​problem/​138"
(deftest test-squares-squared
  )

;; "/​problem/​140"
(deftest test-veitch-please
  )

;; "/​problem/​141"
(deftest test-tricky-card-games
  )

;; "/​problem/​143"
(deftest test-dot-product
  )

;; "/​problem/​144"
(deftest test-oscilrate
  )

;; "/​problem/​145"
(deftest test-for-the-win
  )

;; "/​problem/​146"
(deftest test-trees-into-tables
  )

;; "/​problem/​147"
(deftest test-pascals-trapezoid
  )

;; "/​problem/​148"
(deftest test-the-big-divide
  )

;; "/​problem/​150"
(deftest test-palindromic-numbers
  )

;; "/​problem/​152"
(deftest test-latin-square-slicing
  )

;; "/​problem/​153"
(deftest test-pairwise-disjoint-sets
  )

;; "/​problem/​156"
(deftest test-map-defaults
  )

;; "/​problem/​157"
(deftest test-indexing-sequences
  )

;; "/​problem/​158"
(deftest test-decurry
  )

;; "/​problem/​161"
(deftest test-subset-and-superset
  )

;; "/​problem/​162"
(deftest test-logical-falsity-and-truth
  )

;; "/​problem/​164"
(deftest test-language-of-a-dfa
  )

;; "/​problem/​166"
(deftest test-comparisons
  )

;; "/​problem/​168"
(deftest test-infinite-matrix
  )

;; "/​problem/​171"
(deftest test-intervals
  )

;; "/​problem/​173"
(deftest test-intro-to-destructuring-2
  )

;; "/​problem/​177"
(deftest test-balancing-brackets
  )

;; "/​problem/​178"
(deftest test-best-hand
  )

;; "/​problem/​195"
(deftest test-parentheses-again
  )
