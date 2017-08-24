(ns searcher.generator
  (:gen-class)
  (:require [clojure.string :as string]))


(defn random-letter [letter]
  {:letter letter :placed false})

(defn placed-letter [letter]
  {:letter letter :placed true})

(defn random-letter-coll [coll]
  (into [] (map random-letter coll)))

(defn nil-array [n]
  (into [] (repeatedly n (fn [] nil))))

(defn nil-matrix [columns rows]
  {:matrix (into [] (repeatedly columns #(nil-array rows))) :view :column})

(defn test-matrix []
  ;{:matrix (into [] (map random-letter-coll [["C" "A" "T" "X"] ["X" "Z" "T" "X"] ["Y" "O" "T" "X"]])) :view :column})
  {:matrix [["C" "A" "T" "X"] ["X" "Z" "T" "X"] ["Y" "O" "T" "X"]] :view :column})

(defn- words-can-fit?
  [larger-side smaller-side max-word-size word-count]
  (if
    (>= max-word-size larger-side)
    (cond
     (> word-count smaller-side) false
     (> max-word-size larger-side) false
     :else true)
    (cond
     (> word-count larger-side) false
     (> max-word-size smaller-side) false
     :else true)))


(defn valid? [matrix words]
  (cond
   (nil? matrix) true
   (empty matrix) true
   (nil? words) true
   (empty? words) true
   :else
   (let [dimensions [(count (:matrix matrix)) (count (first (:matrix matrix)))]
         larger-side (apply max dimensions)
         smaller-side  (apply min dimensions)
         max-word-size (apply max (map count words))
         word-count (count words)
         char-count-diff (- (reduce + (map count (:matrix (test-matrix)))) (reduce + (map count words)))]
     (words-can-fit?
      larger-side
      smaller-side
      max-word-size
      word-count))))

(defn non-zeros [coll]
  (cond
   (nil? coll) nil
   (empty? coll) nil
   :else
   (->> coll
        (filter #(not (zero? %))))))

(defn nil-or-empty? [x]
  (or (nil? x) (empty? x)))

(defn min-non-zeros [coll]
  (let [results (non-zeros coll)]
    (if
      (nil-or-empty? results) nil
      (apply min results))))

(defn get-dimensions [raw-matrix]
  (let [dimensions (-> '()
      (conj (count raw-matrix))
      (conj (count (first raw-matrix)))
      (sort))]
    {:small (first dimensions) :large (second dimensions)}))

(defn get-free-matrix [raw-matrix words]
  (let [dimensions (get-dimensions raw-matrix)
        sorted-words (reverse (sort-by count words))
        free-space-counts (map #(- (dimensions :large) (count %)) sorted-words)
        available-free-counts (take (dimensions :large) free-space-counts) ;the remaining free space available in the matrix
        remaining-words (drop (dimensions :large) sorted-words)
        minimum-free-count (min-non-zeros available-free-counts)
        count-free-count (count (non-zeros available-free-counts))]
  nil
    ))

(def test-matrix-1
  (atom (test-matrix)))

(defn rand-char []
  (rand-nth '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "o" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

(defn rand-array [length]
  (repeatedly length #(rand-char)))

(defn rand-2d-array-rec [height, ct, acc]
  (cond
   (not (pos? ct)) acc
   :else (rand-2d-array-rec height (dec ct) (conj acc (rand-array height)))))

(defn rand-2d-array [length, height]
  (rand-2d-array-rec height length '()))

(defn get-col [matrix n]
  (cond
   (= :column (matrix :view))
   (nth (matrix :matrix) n)
   (= :row (matrix :view))
   (get-col (view-swap matrix) n)))

(defn get-row [matrix n]
  (cond
   (= :row (matrix :view))
   (nth (matrix :matrix) n)
   (= :column (matrix :view))
   (get-row (view-swap matrix) n)))

(defn rec-replace [coll word-coll index]
  "returns nil if placement goes out of bounds"
  (cond
   (neg? index) nil
   (> (+ index (count word-coll)) (count coll)) nil
   (empty? word-coll) coll
   (>= index (count coll)) coll
   :else (rec-replace
          (assoc coll index (first word-coll))

          (rest word-coll)
          (inc index))))

(defn place-word [matrix word start-col start-row]
  (cond
   (neg? start-col) nil
   (neg? start-row) nil
   (> start-col (count matrix)) nil
   :else
   (let [result (rec-replace
                 (get-col matrix start-col)
                 (string/split word  #"")
                 start-row)]
     (if (nil? result) nil
       (assoc-in matrix [:matrix start-col] result)))))

(defn traverse-row [matrix, n]
  (map #(nth % n) matrix))

(defn matrix-loop [matrix func row-or-column]
  (let [loop-size (cond
                   (= :row row-or-column)
                   (count (first matrix))
                   (= :column row-or-column)
                   (count matrix))]
    (for [i (range loop-size)] (func matrix i))))


(defn view-swap [matrix]
  (let [new-matrix (matrix-loop (matrix :matrix) (partial traverse-row) :row)]
    (cond
     (= (matrix :view) :column)
     {:matrix new-matrix :view :row}
     (= (matrix :view) :row)
     {:matrix new-matrix :view :column}
     :else nil)))
