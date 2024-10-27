(ns space-invaders.core
  (:require [clansi.core :refer [style]]
            [clj-fuzzy.metrics :as fuzzy-metrics]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [fipp.edn :refer [pprint]]))

;; patterns

(defn validate-text-pattern [pattern-str]
  (let [pattern-lines (str/split-lines pattern-str)]
    (cond
      (seq (mapcat #(remove #{\- \o} %) pattern-lines))
      {:error/msg  "Pattern must contain only valid characters"
       :error/data {:pattern-lines pattern-lines}}
      ;;
      (not= 1 (count (set (map count pattern-lines))))
      {:error/msg  "Pattern lines have to be of the same length"
       :error/data {:pattern-lines pattern-lines}})))

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
       (drop idy) (take height)
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

(defn find-matches
  ([pattern-str text]
   (find-matches pattern-str text nil))
  ([pattern-str text {:keys [min-accuracy]
                      :or   {min-accuracy 99.8}
                      :as   _opts}]
   (let [text-char-seqs   (text-str->char-seqs text)
         [text-width text-height] (text-dimensions text)
         [pattern-width pattern-height
          :as pattern-dims] (text-dimensions pattern-str)
         pattern-size     (* pattern-width pattern-height)
         pattern-char-seq (pattern->char-seq pattern-str)]
     ;; TODO: Account for edge cases. Invaders may fit partially along the text borders.
     (for [idy (range (- text-height pattern-height))
           idx (range (- text-width pattern-width))
           :let [location         [idx idy]
                 subtext-char-seq (extract-subtext-char-seq text-char-seqs
                                                            idx idy
                                                            pattern-width pattern-height)
                 distance         (calc-distance pattern-char-seq subtext-char-seq)
                 accuracy         (- 100.0 (/ distance pattern-size))]
           :when (<= min-accuracy accuracy)]
       {:match/pattern  {:pattern/text pattern-str
                         :pattern/dims pattern-dims}
        :match/location location
        :match/distance distance
        :match/accuracy accuracy
        :match/char-seq subtext-char-seq}))))

;; main logic (high-level)

(defn- find-invader
  [{:invader/keys [pattern] :as _invader} radar-sample opts]
  (if-some [{:error/keys [msg data]} (validate-text-pattern pattern)]
    (do (println (style msg :red))
        (println (style (with-out-str (pprint data)) :red))
        nil)
    (find-matches pattern radar-sample opts)))

(defn find-invaders
  ([invaders radar-sample]
   (find-invaders invaders radar-sample nil))
  ([invaders radar-sample {:keys [sensitivity] :as _opts}]
   (let [opts' (cond-> {}
                       (some? sensitivity) (assoc :min-accuracy sensitivity))]
     (reduce (fn [res {invader-type :invader/type :as invader}]
               (let [matches (find-invader invader radar-sample opts')]
                 (if (seq matches)
                   (assoc res invader-type matches)
                   res)))
             {}
             invaders))))

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
      (println (format "Found %s possible '%s' invader matches:"
                       (count matches) (name invader-type)))
      (doseq [output-match (->> matches
                                (sort-by :match/distance)
                                (map #(->output-match %)))]
        (pprint output-match))
      (println))
    (println "No invaders were found.")))

(def cli-options-spec
  [["-s" "--sensitivity SENSITIVITY"
    "Search sensitivity in percent between 0 (exclusive) and 100 (inclusive)."
    :default 99.8
    :parse-fn Float/parseFloat
    :validate [#(and (float? %) (< 0 %) (<= % 100))
               "Must be a floating point number in the range (0 .. 100]."]]
   ["-h" "--help"]])

;; TODO: Implement the '--help' CLI argument processing.
(defn -main [& args]
  (let [opts    (:options (cli/parse-opts args cli-options-spec))
        results (find-invaders invaders (prepare-text radar-sample) opts)]
    (print-results results)
    nil))

(comment
  (-main)
  (-main "--sensitivity" "99.7")
  .)

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
