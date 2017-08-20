(ns searcher.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;(use '[clojure.string :as string])

(defn word-list []
  '("OX","CAT","TOY","AT","DOG","CATAPULT","T"))

(defn test-matrix []
  '(["C" "A" "T"] ["X" "Z" "T"] ["Y" "O" "T"]))

(defn single-array []
  ["a" "b" "c" "a" "t" "z" "p"])

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

(defn contains-word? [vect, word]
  (-> (reduce str vect)
      (string/includes? word)))

(defn occurence-count [vect word]
  (->> (reduce str vect)
       (re-seq (re-pattern word))
     count))

(defn get-col [matrix, n]
  (nth matrix n))

(defn get-row [matrix, n]
  (map #(nth % n) matrix))

(defn get-position [matrix, x, y]
  (cond
   (>= x (count matrix)) nil
   (>= y (count (first matrix))) nil
   :else
   (-> (get-col matrix x)
       (nth y))))

(defn get-diag-rec [matrix x y acc]
  (cond
   (nil? (get-position matrix x y)) acc
   :else
   (get-diag-rec matrix (inc x) (inc y) (conj acc (get-position matrix x y)))))

(defn get-diag-col [matrix n]
  ;pass in a row header number
  (get-diag-rec matrix n 0 []))

(defn get-diag-row [matrix n]
  (get-diag-rec matrix 0 n []))

(defn matrix-loop [matrix func row-or-column]
  (let [loop-size (cond
                   (= :row row-or-column)
                   (count (first matrix))
                   (= :column row-or-column)
                   (count matrix))]
    (for [i (range loop-size)] (func matrix i))))


(defn get-all-rows [matrix]
  (matrix-loop matrix (partial get-row) :row))
;(for [i (range (count (first matrix)))] (get-row matrix i)))

(defn get-all-columns [matrix]
  ;redundant if matrix is represented as a coll of colls
  (for [i (range (count  matrix))] (get-col matrix i)))

(defn get-top-diagonals [matrix]
  (for [i (range (count  matrix))] (get-diag-col matrix i)))

(defn get-bottom-diagonals [matrix]
  (for [i (range 1 (count (first matrix)))] (get-diag-row matrix i)))

(defn get-all-sequences [matrix]
  (-> '()
      (into (get-all-rows matrix))
      (into (get-all-columns matrix))
      (into (get-top-diagonals matrix))
      (into (get-bottom-diagonals matrix))))

(get-all-sequences (test-matrix))

(defn sequences-occurence-count [sequences word]
  (->> (map #(occurence-count % word) sequences)))

(defn word-occurence [sequences word]
  (let [ct (sequences-occurence-count sequences word)]
    (reduce + ct)))

;special handling for characters
(defn character-count [matrix character]
   (let [flat-matrix (->> matrix
                         (flatten)
                         (reduce str))]
    (-> (re-seq (re-pattern character) flat-matrix)
        count)))

;special handling for characters
(defn character-occurences [matrix characters]
  (->> (map
   (fn [arg]
     {arg (character-count matrix arg)}) characters)
  (reduce merge)))

(defn word-occurences [matrix words]
  (let [sequences (get-all-sequences matrix)]
    (->> (map
          (fn [arg] {arg
                     (+ (word-occurence sequences (string/reverse arg))
                        (word-occurence sequences arg))})
          words)
         (reduce merge))))

(defn reverse-words [words]
  (map string/reverse words))

(defn multi-char-words [words]
  (filter #(> (count %) 1) words))

(defn single-char-words [words]
  (filter #(= (count %) 1) words))

(defn find-words [matrix words]
  (let [single-char (single-char-words words)
        multi-char (multi-char-words words)]
    (conj (word-occurences matrix multi-char)
         (character-occurences matrix single-char))))

(find-words (test-matrix) (word-list))
