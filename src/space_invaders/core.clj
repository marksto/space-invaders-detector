(ns space-invaders.core
  (:require [clj-fuzzy.metrics :as fuzzy-metrics]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [fipp.edn :refer [pprint]]))

;; patterns

;; TODO: Implement an invader text pattern validation.
(defn validate-text-pattern [_pattern-str])

(defn pattern->char-seq [pattern-str]
  (remove #{\newline} (seq pattern-str)))

;; texts (input strings)

;; TODO: Implement a text/input string preparation (padding, etc.).
(defn prepare-text [text]
  text)

(defn text-str->char-seqs [text-str]
  (map seq (str/split-lines text-str)))

(defn extract-subtext-char-seq
  [char-seqs idx idy width height]
  (->> char-seqs
       (drop idy)
       (take height)
       (mapcat #(->> % (drop idx) (take width)))))

;; pattern matching & calcs

(defn text-dimensions [text-str]
  (let [text-lines (str/split-lines text-str)]
    [(count (first text-lines)) (count text-lines)]))

(defn calc-distance
  [char-seq-1 char-seq-2]
  (assert (= (count char-seq-1) (count char-seq-2))
          "The Hamming distance is only defined for seqs of the same length")
  (fuzzy-metrics/hamming char-seq-1 char-seq-2))

#_(defn matches-subtext-at-loc?
    [pattern-char-seq text-char-seqs
     [idx idy :as _loc]
     [pattern-width pattern-height :as _pattern-dims]
     distance-threshold]
    (let [subtext-char-seq (extract-subtext-char-seq text-char-seqs
                                                     idx idy
                                                     pattern-width pattern-height)
          distance         (calc-distance pattern-char-seq subtext-char-seq)]
      (<= distance distance-threshold)))

#_(defn pattern-subtext-distance
    [pattern-char-seq text-char-seqs
     [idx idy :as _loc]
     pattern-width pattern-height]
    (let [subtext-char-seq (extract-subtext-char-seq text-char-seqs
                                                     idx idy
                                                     pattern-width pattern-height)]
      (calc-distance pattern-char-seq subtext-char-seq)))

(defn find-matches
  [pattern-str text]
  (let [[pattern-width pattern-height
         :as pattern-dims] (text-dimensions pattern-str)
        pattern-size     (* pattern-width pattern-height)
        pattern-char-seq (pattern->char-seq pattern-str)
        text-char-seqs   (text-str->char-seqs text)
        [text-width text-height] (text-dimensions text)]
    ;; TODO: Account for edge cases. Invaders may fit partially along the text borders.
    (for [idy (range (- text-height pattern-height))
          idx (range (- text-width pattern-width))
          :let [location         [idx idy]
                subtext-char-seq (extract-subtext-char-seq text-char-seqs
                                                           idx idy
                                                           pattern-width pattern-height)
                distance         (calc-distance pattern-char-seq subtext-char-seq)
                accuracy         (- 100.0 (/ distance pattern-size))]
          :when (<= 99.8 accuracy)]
      {:match/pattern  {:pattern/text pattern-str
                        :pattern/dims pattern-dims}
       :match/location location
       :match/distance distance
       :match/accuracy accuracy
       :match/char-seq subtext-char-seq})))

(comment
  (let [[idx idy] [74 1]
        [pattern-width pattern-height] [11 8]]
    (->> (extract-subtext-char-seq (text-str->char-seqs radar-sample)
                                   idx idy
                                   pattern-width pattern-height)
         (partition pattern-width)
         (mapv #(apply str %))))
  .)

;; main logic (high-level)

;; TODO: Impl configurable "sensitivity" of the search.

(defn find-invader
  [{:invader/keys [pattern] :as _invader} radar-sample]
  (find-matches pattern radar-sample))

(defn find-invaders
  [invaders radar-sample]
  (reduce (fn [res {invader-type :invader/type :as invader}]
            (let [matches (find-invader invader radar-sample)]
              (if (seq matches)
                (assoc res invader-type matches)
                res)))
          {}
          invaders))

;; I/O and entrypoint

(defn read-text-file [file-path]
  (slurp (io/resource (format "space_invaders/%s" file-path))))

(def invaders
  [{:invader/type    :invader.type/squid
    :invader/pattern (read-text-file "invaders/squid.txt")}
   {:invader/type    :invader.type/crab
    :invader/pattern (read-text-file "invaders/crab.txt")}])

(def radar-sample (read-text-file "radar_samples/sample-1.txt"))

(defn ->output-match
  [{:match/keys [pattern location distance accuracy char-seq] :as _match}]
  {:location location
   #_#_:distance distance
   :accuracy (format "%.2f%%" accuracy)
   :matching (->> char-seq
                  (partition (-> pattern :pattern/dims first))
                  (mapv str/join))})

(defn print-results [results]
  (if-some [res-seq (seq results)]
    (doseq [[invader-type matches] res-seq]
      (println (format "Found %s possible '%s' invader matches"
                       (count matches) (name invader-type)))
      (doseq [output-match (->> matches
                                (sort-by :match/distance)
                                (map #(->output-match %)))]
        (pprint output-match))
      (println))
    (println "Nobody found")))

(defn -main [& _args]
  (doseq [invader invaders]
    (validate-text-pattern (:invader/pattern invader)))
  (let [results (find-invaders invaders (prepare-text radar-sample))]
    (print-results results)))

;;

(comment
  (require '[criterium.core :refer [bench quick-bench]])

  (quick-bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 36 in 6 samples of 6 calls.
  ;;           Execution time mean : 20,181756 ms
  ;;  Execution time std-deviation : 4,439010 ms
  ;; Execution time lower quantile : 17,211264 ms ( 2,5%)
  ;; Execution time upper quantile : 27,602358 ms (97,5%)
  ;;                 Overhead used : 7,857827 ns
  (bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 3540 in 60 samples of 59 calls.
  ;;           Execution time mean : 17,640670 ms
  ;;  Execution time std-deviation : 397,032998 µs
  ;; Execution time lower quantile : 17,161339 ms ( 2,5%)
  ;; Execution time upper quantile : 18,687544 ms (97,5%)
  ;;                 Overhead used : 7,857827 ns
  .)
