(ns four-clojure.core
  (:require [clojure.pprint :refer [pprint]]))

;; "/​problem/​1"
(def nothing-but-the-truth)

;; "/​problem/​2"
(def simple-math)

;; "/​problem/​3"
(def intro-to-strings)

;; "/​problem/​4"
(def intro-to-lists)

;; "/​problem/​5"
(def lists-conj)

;; "/​problem/​6"
(def intro-to-vectors)

;; "/​problem/​7"
(def vectors-conj)

;; "/​problem/​8"
(def intro-to-sets)

;; "/​problem/​9"
(def sets-conj)

;; "/​problem/​10"
(def intro-to-maps
  20)

;; "/​problem/​11"
(def maps-conj
  {:b 2})

;; "/​problem/​12"
(def intro-to-sequences)

;; "/​problem/​13"
(def sequences-rest)

;; "/​problem/​14"
(def intro-to-functions)

;; "/​problem/​15"
(def double-down
  (partial * 2))

;; "/​problem/​16"
(def hello-world
  #(str "Hello, " % "!"))

;; "/​problem/​17"
(def sequences-map)

;; "/​problem/​18"
(def sequences-filter)

;; "/​problem/​19"
(def last-element
  #(-> % reverse first))

;; "/​problem/​20"
(def penultimate-element
  #(-> % reverse second))

;; "/​problem/​21"
(def nth-element
  #((into [] %1) %2))

;; "/​problem/​22"
(def count-a-sequence
  #(reduce (fn [n _] (inc n)) 0 %))

;; "/​problem/​23"
(def reverse-a-sequence
  (fn [s]
    (reduce #(cons %2 %1) [] s)))

;; "/​problem/​24"
(def sum-it-all-up
  #(apply + %))

;; "/​problem/​25"
(def find-the-odd-numbers
  #(remove (fn [n] (zero? (mod n 2))) %))

;; "/​problem/​26"
(def fibonacci-sequence
  (fn [n]
    (->> (iterate #(vector (second %) (reduce + %)) [0 1])
         (map second)
         (take n))))

;; "/​problem/​27"
(def palindrome-detector
  #(if (instance? String %)
     (= (apply str (reverse %)) %)
     (= (reverse %) %)))

;; "/​problem/​28"
(def flatten-a-sequence
  (fn [coll]
    (let [l (first coll), r (rest coll)]

      (concat
       (if (sequential? l)
         (flatten-a-sequence l)
         (vector l))

       (when-not (empty? r)
         (if (sequential? r)
           (flatten-a-sequence r)
           (vector r)))))))

;; "/​problem/​29"
(def get-the-caps
  (fn [s]
    (apply str (filter #(re-find #"[A-Z]" (str %)) s))))

;; "/​problem/​30"
(def compress-a-sequence
  #(->> (partition-by identity %)
        (map first)))

;; "/​problem/​31"
(def pack-a-sequence
  #(partition-by identity %))

;; "/​problem/​32"
(def duplicate-a-sequence
  (fn [s] (mapcat #(vector % %) s)))

;; "/​problem/​33"
(def replicate-a-sequence
  (fn [s n] (mapcat #(repeat n %) s)))

;; "/​problem/​34"
(def implement-range
  (fn [lb ub]
    (take-while #(< % ub) (iterate inc lb))))

;; "/​problem/​35"
(def local-bindings)

;; "/​problem/​36"
(def let-it-be)

;; "/​problem/​37"
(def regular-expressions)

;; "/​problem/​38"
(def maximum-value
  (fn [& s]
    (reduce #(if (> %1 %2) %1 %2) s)))

;; "/​problem/​39"
(def interleave-two-seqs
  #(mapcat vector %1 %2))

;; "/​problem/​40"
(def interpose-a-seq
  #_(fn [elem coll] (drop-last (mapcat vector coll (repeat elem))))
  (fn [e coll]
    (concat [(first coll)]
            (mapcat #(vector e %) (rest coll)))))

;; "/​problem/​41"
(def drop-every-nth-item
  (fn [coll n]
    (let [pad-with (repeat (- n (mod (count coll) n)) nil)]

      (->> (partition (dec n) n (concat coll pad-with))
           flatten
           (remove nil?)))))

;; "/​problem/​42"
(def factorial-fun
  #(reduce *' (range 1 (inc %))))

;; "/​problem/​43"
(def reverse-interleave
  (fn [s n]
    (apply map list (partition n s))))

;; "/​problem/​44"
(def rotate-sequence
  (fn [n coll]
    (let [v (into [] coll)
          n (mod n (count v))]
      (concat (subvec v n) (subvec v 0 n)))))

;; "/​problem/​45"
(def intro-to-iterate)

;; "/​problem/​46"
(def flipping-out
  (fn [f] #(f %2 %1)))

;; "/​problem/​47"
(def contain-yourself)

;; "/​problem/​48"
(def intro-to-some)

;; "/​problem/​49"
(def split-a-sequence
  (fn [n coll]
    [(subvec coll 0 n) (subvec coll n)]))

;; "/​problem/​50"
(def split-by-type
  #(vals (group-by class %)))

;; "/​problem/​51"
(def advanced-destructuring)

;; "/​problem/​52"
(def intro-to-destructuring)

;; "/​problem/​53"
(def longest-increasing-sub-seq
  (fn [s]
    (->> (partition 2 1 s)
         (partition-by #(pos? (- (second %) (first %))))
         (reduce #(if (< (count %1) (count %2)) %2 %1))
         (remove #(< (second %) (first %)))
         flatten
         distinct)))

;; "/​problem/​54"
(def partition-a-sequence
  (fn [n coll]
    (when (<= n (count coll))
      (cons (take n coll) (partition-a-sequence n (drop n coll))))))

;; "/​problem/​55"
(def count-occurrences
  (fn [coll]
    (->> (group-by identity coll)
         (reduce-kv (fn [m k v] (assoc m k (count v))) {}))))

;; "/​problem/​56"
(def find-distinct-items
  #(loop [coll %
          distinct-items []]

     (if (empty? coll)
       distinct-items
       (let [next-item (first coll)]
         (recur (remove #{next-item} coll) (conj distinct-items next-item))))))

;; "/​problem/​57"
(def simple-recursion)

;; "/​problem/​58"
(def function-composition
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %1)
              (apply (last fs) args)
              (rest (reverse fs))))))

;; "/​problem/​59"
(def juxtaposition
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

;; "/​problem/​60"
(def sequence-reductions
  (fn
    ([f coll] (sequence-reductions f (first coll) (rest coll)))
    ([f val coll]
     (cons val (lazy-seq
                (when-let [s (seq coll)]
                  (sequence-reductions f (f val (first s)) (rest s))))))))

;; "/​problem/​61"
(def map-construction
  #(into {} (apply map hash-map %&)))

;; "/​problem/​62"
(def reimplement-iterate
  (fn [f n]
    (cons n (lazy-seq (reimplement-iterate f (f n))))))

;; "/​problem/​63"
(def group-a-sequence
  (fn [f coll]
    (reduce (fn [m x]
              (let [f-x (f x)]
                (assoc-in m [f-x] (into [] (conj (m f-x) x))))) {} coll)))

;; "/​problem/​64"
(def intro-to-reduce
  +)

;; "/​problem/​65"
(def black-box-testing)

;; "/​problem/​66"
(def greatest-common-divisor
  (fn [l r]
    (let [divisors (fn [n]
                     (into #{} (filter #(zero? (mod n %)) (range 1 (+ 1 n)))))]
      (apply max (clojure.set/intersection (divisors l)
                                           (divisors r))))))

;; "/​problem/​67"
(def prime-numbers
  (fn [n]
    (->> (range)
         (filter #(.isProbablePrime (BigInteger/valueOf %) 5))
         (take n))))

;; "/​problem/​68"
(def recurring-theme)

;; "/​problem/​69"
(def merge-with-a-function
  (fn [f & ms]
    (reduce #(conj % (reduce (fn [r [k v]]
                               (let [rv (get r k)]
                                 (assoc r k (if rv
                                              (f rv v)
                                              v)))) %1 %2))
            ms)))

;; "/​problem/​70"
(def word-sorting
  (fn [sentence]
    (let [lower-case-sentence (-> sentence
                                  (clojure.string/replace #"[\.\!\?]" "")
                                  (clojure.string/split #"\s"))]
      (sort-by #(clojure.string/lower-case %) lower-case-sentence))))

;; "/​problem/​71"
(def rearranging-code-->)

;; "/​problem/​72"
(def rearranging-code-->>)

;; "/​problem/​73"
(def analyze-a-tic-tac-toe-board
  (fn [board]
    (let [have-winner? (fn [three-marks]
                         (let [d (distinct three-marks)]
                           (when (and (not= [:e] d)
                                      (= (count d) 1))
                             (apply identity d))))
          rotated-board (apply map list board)
          get-elem-at (fn [x y] (nth (nth board x) y))
          diagonals [[(get-elem-at 0 0) (get-elem-at 1 1) (get-elem-at 2 2)]
                     [(get-elem-at 0 2) (get-elem-at 1 1) (get-elem-at 2 0)]]]
      (or (some have-winner? board)
          (some have-winner? rotated-board)
          (some have-winner? diagonals)))))

;; "/​problem/​74"
(def filter-perfect-squares
  (fn [str] (clojure.string/join
             ","
             (let [coll (map #(Integer/parseInt %) (clojure.string/split str #","))]
               (let [perfect-squares (map #(* % %) (iterate inc 1))]
                 (filter (fn [elm] (some #(= elm %) (take (Math/sqrt elm) perfect-squares))) coll))))))

;; "/​problem/​75"
(def eulers-totient-function
  (fn [n]
    (let [gcd #(if (zero? %2)
                 %1
                 (recur %2 (mod %1 %2)))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

;; "/​problem/​76"
(def intro-to-trampoline)

;; "/​problem/​77"
(def anagram-finder
  (fn [words]
    (->> words
         (group-by #(sort %))
         vals
         (remove #(= 1 (count %)))
         (map set)
         (into #{}))))

;; "/​problem/​78"
(defn reimplement-trampoline
  ([f] (let [retval (f)]
         (if (fn? retval)
           (recur retval)
           retval)))
  ([f & args] (reimplement-trampoline #(apply f args))))

;; "/​problem/​79"
(def triangle-minimal-path
  (fn [triangle]
    (let [reduce-triangle (fn [t]
                            (let [row-to-reduce (t (- (count t) 2))
                                  leaf-row (t (dec (count t)))]
                              (conj (subvec t 0 (- (count t) 2))
                                    (into [] (for [i (range (count row-to-reduce))]
                                               (+ (row-to-reduce i)
                                                  (min (leaf-row i) (leaf-row (inc i)))))))))]
      (->> triangle
           (into [])
           (iterate reduce-triangle)
           (take (count triangle))
           last
           ffirst))))

;; "/​problem/​80"
(defn perfect-numbers [n]
  (= n (reduce + (filter #(zero? (mod n %)) (range 1 (inc (quot n 2)))))))

;; "/​problem/​81"
(defn set-intersection [ls rs]
  (->> ls
       (filter #(contains? rs %))
       (into #{})))

;; "/​problem/​82"
(defn word-difference [lhs rhs]
  (if (= (count lhs) (count rhs))
    (->> (map #(= %1 %2) lhs rhs) (remove true?) count)
    (let [[smaller larger] (sort-by count [lhs rhs])]
      (loop [smaller smaller,
             larger larger,
             num-diffs 0]
        (cond (and (clojure.string/blank? smaller) (clojure.string/blank? larger)) num-diffs

              (or (clojure.string/blank? smaller) (clojure.string/blank? larger)) (+ num-diffs (max (count smaller) (count larger)))
              :else (if (= (.charAt smaller 0)
                           (.charAt larger 0))
                      (recur (subs smaller 1) (subs larger 1) num-diffs)
                      (recur smaller (subs larger 1) (inc num-diffs))))))))

(defn- get-sibling-map [words]
  (reduce (fn [m k] (assoc m k (filter #(= 1 (word-difference k %)) words))) {} words))

(defn word-chains
  "each word differs by only one letter from the words directly before and after it. The one letter difference can be either an insertion, a deletion, or a substitution"
  [words]
  (let [sibling-map (get-sibling-map words)]
    (println "sibling-map : " sibling-map)
    (loop [used-words #{}
           remaining-words words
           current-word (first remaining-words)]

      ;; (println "used-words : " used-words)
      ;; (println "remaining-words : " remaining-words)
      ;; (println "current-word : " current-word)

      (cond
        (= (count words) (count used-words)) true
        ;; (empty? remaining-words) false
        (nil? current-word) false
        :else (let [siblings (remove #(contains? #{%} used-words) (get sibling-map current-word))]
                ;; (println "siblings : " siblings)
                ;; (println "\n")

                (recur (into #{} (concat used-words siblings)) (rest remaining-words) (first remaining-words)))))))

;; "/​problem/​83"
(def a-half-truth
  #(boolean (and (some true? %&) (not (every? true? %&)))))

;; "/​problem/​84"
(def transitive-closure)

;; "/​problem/​85"
(def power-set)

;; "/​problem/​86"
(def happy-numbers)

;; "/​problem/​88"
(def symmetric-difference)

;; "/​problem/​89"
(def graph-tour)

;; "/​problem/​90"
(def cartesian-product)

;; "/​problem/​91"
(def graph-connectivity)

;; "/​problem/​92"
(def read-roman-numerals)

;; "/​problem/​93"
(def partially-flatten-a-sequence)

;; "/​problem/​94"
(def game-of-life)

;; "/​problem/​95"
(def to-tree-or-not-to-tree)

;; "/​problem/​96"
(def beauty-is-symmetry)

;; "/​problem/​97"
(def pascals-triangle)

;; "/​problem/​98"
(def equivalence-classes)

;; "/​problem/​99"
(def product-digits)

;; "/​problem/​100"
(def least-common-multiple)

;; "/​problem/​101"
(def levenshtein-distance)

;; "/​problem/​102"
(def intocamelcase)

;; "/​problem/​103"
(def generating-k-combinations)

;; "/​problem/​104"
(def write-roman-numerals)

;; "/​problem/​105"
(def identify-keys-and-values)

;; "/​problem/​106"
(def number-maze)

;; "/​problem/​107"
(def simple-closures)

;; "/​problem/​108"
(def lazy-searching)

;; "/​problem/​110"
(def sequence-of-pronunciations)

;; "/​problem/​111"
(def crossword-puzzle)

;; "/​problem/​112"
(def sequs-horribilis)

;; "/​problem/​113"
(def making-data-dance)

;; "/​problem/​114"
(def global-take-while)

;; "/​problem/​115"
(def the-balance-of-n)

;; "/​problem/​116"
(def prime-sandwich)

;; "/​problem/​117"
(def for-science!)

;; "/​problem/​118"
(def re-implement-map)

;; "/​problem/​119"
(def win-at-tic-tac-toe)

;; "/​problem/​120"
(def sum-of-square-of-digits)

;; "/​problem/​121"
(def universal-computation-engine)

;; "/​problem/​122"
(def read-a-binary-number)

;; "/​problem/​124"
(def analyze-reversi)

;; "/​problem/​125"
(def gus-quinundrum)

;; "/​problem/​126"
(def through-the-looking-class)

;; "/​problem/​127"
(def love-triangle)

;; "/​problem/​128"
(def recognize-playing-cards)

;; "/​problem/​130"
(def tree-reparenting)

;; "/​problem/​131"
(def sum-some-set-subsets)

;; "/​problem/​132"
(def insert-between-two-items)

;; "/​problem/​134"
(def a-nil-key)

;; "/​problem/​135"
(def infix-calculator)

;; "/​problem/​137"
(def digits-and-bases)

;; "/​problem/​138"
(def squares-squared)

;; "/​problem/​140"
(def veitch-please)

;; "/​problem/​141"
(def tricky-card-games)

;; "/​problem/​143"
(def dot-product)

;; "/​problem/​144"
(def oscilrate)

;; "/​problem/​145"
(def for-the-win)

;; "/​problem/​146"
(def trees-into-tables)

;; "/​problem/​147"
(def pascals-trapezoid)

;; "/​problem/​148"
(def the-big-divide)

;; "/​problem/​150"
(def palindromic-numbers)

;; "/​problem/​152"
(def latin-square-slicing)

;; "/​problem/​153"
(def pairwise-disjoint-sets)

;; "/​problem/​156"
(def map-defaults)

;; "/​problem/​157"
(def indexing-sequences)

;; "/​problem/​158"
(def decurry)

;; "/​problem/​161"
(def subset-and-superset)

;; "/​problem/​162"
(def logical-falsity-and-truth)

;; "/​problem/​164"
(def language-of-a-dfa)

;; "/​problem/​166"
(def comparisons)

;; "/​problem/​168"
(def infinite-matrix)

;; "/​problem/​171"
(def intervals)

;; "/​problem/​173"
(def intro-to-destructuring-2)

;; "/​problem/​177"
(def balancing-brackets)

;; "/​problem/​178"
(def best-hand)

;; "/​problem/​195"
(def parentheses-again)
