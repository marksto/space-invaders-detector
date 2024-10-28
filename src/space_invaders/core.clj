(ns space-invaders.core
  (:require [clansi.core :refer [style]]
            [clj-fuzzy.metrics :as fuzzy-metrics]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [fipp.edn :refer [pprint]]))

;; patterns

(defn validate-pattern-str [pattern-str]
  (let [pattern-lines (str/split-lines pattern-str)]
    (cond
      (seq (mapcat #(remove #{\- \o} %) pattern-lines))
      {:error/msg  "Pattern must contain only valid characters"
       :error/data {:pattern-lines pattern-lines}}
      ;;
      (not= 1 (count (set (map count pattern-lines))))
      {:error/msg  "Pattern lines have to be of the same length"
       :error/data {:pattern-lines pattern-lines}})))

#_(defn ->sub-pattern
    [char-seq [width height]]
    (let [char-seqs (partition width char-seq)]
      {:pattern/sub?      true
       :pattern/text      (str/join (flatten (interpose \newline char-seqs)))
       :pattern/dims      [width height]
       :pattern/char-seqs char-seqs}))

;; input strings

;; TODO: Impl an input string preparation (lines padding, dims >= pattern dims).
(defn prepare-input-str [input-str]
  input-str)

;; pattern matching

(defn text-dimensions [text-str]
  (let [text-lines (str/split-lines text-str)]
    [(count (first text-lines)) (count text-lines)]))

(defn text-str->char-seqs [text-str]
  (map seq (str/split-lines text-str)))

(defn text-str->char-seq [text-str]
  (remove #{\newline} (seq text-str)))

(defn extract-char-subseq
  [char-seqs [idx idy] [width height]]
  (->> char-seqs
       (drop idy) (take height)
       (mapcat #(->> % (drop idx) (take width)))))

(defn extract-char-subseq+
  [char-seqs [idx idy] [width height]]
  (->> char-seqs
       (drop idy) (take height)
       (map #(->> % (drop idx) (take width)))))

(defn calc-distance
  [char-seq-1 char-seq-2]
  (assert (= (count char-seq-1) (count char-seq-2))
          "The Hamming distance is only defined for seqs of the same length")
  (fuzzy-metrics/hamming char-seq-1 char-seq-2))

(defn ->pattern-match
  [p-char-seq p-dims
   i-char-seqs i-loc
   min-accuracy]
  (let [i-subseq (extract-char-subseq i-char-seqs i-loc p-dims)
        distance (calc-distance p-char-seq i-subseq)
        accuracy (- 100.0 (/ distance (count p-char-seq)))]
    (when (<= min-accuracy accuracy)
      {:match/location  i-loc
       :match/dimension p-dims
       :match/char-seq  i-subseq
       :match/distance  distance
       :match/accuracy  accuracy})))

(defmulti edge-shifts
  (fn [edge-kind _pattern _min-sub-pattern] edge-kind))

(defmethod edge-shifts :top
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-height min-sub-pattern))
       (range 1)
       (map (fn [y-shift]
              {:shift/pattern-loc  [0 y-shift]
               :shift/pattern-dims [p-width (- p-height y-shift)]}))))

(defmethod edge-shifts :left
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-width min-sub-pattern))
       (range 1 p-width)
       (map (fn [x-shift]
              {:shift/pattern-loc  [x-shift 0]
               :shift/pattern-dims [(- p-width x-shift) p-height]}))))

(defmethod edge-shifts :bottom
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-height min-sub-pattern))
       (range 1)
       (map (fn [y-shift]
              {:shift/pattern-loc  [0 0]
               :shift/pattern-dims [p-width (- p-height y-shift)]
               :shift/input-loc-fn (fn [[idx idy]] [idx (+ idy y-shift)])}))))

(defmethod edge-shifts :right
  [_ {[p-width p-height] :pattern/dims :as _pattern} min-sub-pattern]
  (->> (inc (- p-width min-sub-pattern))
       (range 1)
       (map (fn [x-shift]
              {:shift/pattern-loc  [0 0]
               :shift/pattern-dims [(- p-width x-shift) p-height]
               :shift/input-loc-fn (fn [[idx idy]] [(+ idx x-shift) idy])}))))

(defmulti edge-base-locs
  (fn [edge-kind _pattern _input] edge-kind))

(defmethod edge-base-locs :top
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector % 0)
       (range (inc (- i-width p-width)))))

(defmethod edge-base-locs :left
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector 0 %)
       (range (inc (- i-height p-height)))))

(defmethod edge-base-locs :bottom
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector % (- i-height p-height))
       (range (inc (- i-width p-width)))))

(defmethod edge-base-locs :right
  [_
   {[p-width p-height] :pattern/dims :as _pattern}
   {[i-width i-height] :input/dims :as _input}]
  (map #(vector (- i-width p-width) %)
       (range (inc (- i-height p-height)))))

(defn- find-edge-matches
  [edge-kind
   {p-char-seqs :pattern/char-seqs :as pattern}
   {i-char-seqs :input/char-seqs :as input}
   min-sub-pattern
   min-accuracy]
  (let [edge-shifts    (edge-shifts edge-kind pattern min-sub-pattern)
        edge-base-locs (edge-base-locs edge-kind pattern input)]
    (reduce
      (fn [matches base-loc]
        (if-some [new-match (reduce
                              (fn [_ {p-loc    :shift/pattern-loc
                                      p-dims   :shift/pattern-dims
                                      i-loc-fn :shift/input-loc-fn}]
                                (let [p-char-subseq (extract-char-subseq
                                                      p-char-seqs
                                                      p-loc p-dims)
                                      shifted-i-loc (if (some? i-loc-fn)
                                                      (i-loc-fn base-loc)
                                                      base-loc)]
                                  (when-some [match (->pattern-match
                                                      p-char-subseq p-dims
                                                      i-char-seqs shifted-i-loc
                                                      min-accuracy)]
                                    (reduced match))))
                              nil
                              edge-shifts)]
          (conj matches new-match)
          matches))
      []
      edge-base-locs)))

(defn- find-matches-on-edges
  [pattern input min-sub-pattern min-accuracy]
  (concat (find-edge-matches :top
                             pattern input min-sub-pattern min-accuracy)
          (find-edge-matches :left
                             pattern input min-sub-pattern min-accuracy)
          (find-edge-matches :bottom
                             pattern input min-sub-pattern min-accuracy)
          (find-edge-matches :right
                             pattern input min-sub-pattern min-accuracy)))

(defn- find-full-matches
  [{pattern-str :pattern/text [p-width p-height :as p-dims] :pattern/dims :as _pattern}
   {i-char-seqs :input/char-seqs [i-width i-height] :input/dims :as _input}
   min-accuracy]
  (let [p-char-seq (text-str->char-seq pattern-str)]
    (for [idy (range (inc (- i-height p-height)))
          idx (range (inc (- i-width p-width)))
          :let [match (->pattern-match p-char-seq p-dims
                                       i-char-seqs [idx idy]
                                       min-accuracy)]
          :when (some? match)]
      match)))

(defn find-matches
  {:arglists '([pattern-str input-str]
               [pattern-str input-str {:keys [min-accuracy
                                              search-on-edges
                                              min-sub-pattern]
                                       :or   {min-accuracy    99.8
                                              min-sub-pattern 1}
                                       :as   _opts}])}
  ([pattern-str input-str]
   (find-matches pattern-str input-str nil))
  ([pattern-str input-str {:keys [min-accuracy
                                  search-on-edges
                                  min-sub-pattern]
                           :or   {min-accuracy    99.8
                                  min-sub-pattern 1}
                           :as   _opts}]
   (let [pattern {:pattern/text      pattern-str
                  :pattern/dims      (text-dimensions pattern-str)
                  :pattern/char-seqs (text-str->char-seqs pattern-str)}
         input   {:input/text      input-str
                  :input/dims      (text-dimensions input-str)
                  :input/char-seqs (text-str->char-seqs input-str)}]
     (concat (find-full-matches pattern input min-accuracy)
             (when search-on-edges
               (find-matches-on-edges
                 pattern input min-sub-pattern min-accuracy))))))

;; main logic (high-level)

(defn- prepare-search-opts
  [{:keys [sensitivity edges edges-cut-off] :as _opts}]
  (cond-> {}
          (some? sensitivity) (assoc :min-accuracy sensitivity)
          (true? edges) (assoc :search-on-edges true)
          (some? edges-cut-off) (assoc :min-sub-pattern edges-cut-off)))

(defn- find-invader
  [{:invader/keys [pattern] :as _invader} radar-sample opts]
  (if-some [{:error/keys [msg data]} (validate-pattern-str pattern)]
    (do (println (style msg :red))
        (println (style (with-out-str (pprint data)) :red))
        nil)
    (find-matches pattern radar-sample opts)))

(defn find-invaders
  {:arglists '([invaders radar-sample]
               [invaders radar-sample {:keys [sensitivity edges edges-cut-off]
                                       :as   _opts}])}
  ([invaders radar-sample]
   (find-invaders invaders radar-sample nil))
  ([invaders radar-sample opts]
   (let [radar-sample' (prepare-input-str radar-sample)
         search-opts   (prepare-search-opts opts)]
     (reduce (fn [res {invader-type :invader/type :as invader}]
               (let [matches (find-invader invader radar-sample' search-opts)]
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

(defn- ->output-match
  [{:match/keys [location dimension char-seq distance accuracy] :as _match}]
  {:location location
   :distance distance
   :accuracy (format "%.2f%%" accuracy)
   :matching (->> char-seq
                  (partition (first dimension))
                  (interpose \newline)
                  (flatten)
                  (apply str))})

(defn print-results [results]
  (if-some [res-seq (seq results)]
    (doseq [[invader-type matches] res-seq]
      (println (format "Found %s possible '%s' invader matches:"
                       (count matches) (name invader-type)))
      (doseq [output-match (->> matches
                                (sort-by :match/distance)
                                (map ->output-match))]
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
   ["-e" "--edges ON"
    "Turns off/on the search along edges of the input string (radar sample)."
    :default true
    :parse-fn Boolean/parseBoolean]
   [nil "--edges-cut-off LINES"
    "Sets a cut-off (minimum number of lines) for searching along the edges."
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [pos-int? "Must be a positive natural number."]]
   ["-h" "--help"]])

;; TODO: Implement the '--help' CLI argument processing.
(defn -main [& args]
  (let [opts    (:options (cli/parse-opts args cli-options-spec))
        results (find-invaders invaders radar-sample opts)]
    (print-results results)
    nil))

(comment
  (-main)
  (-main "--sensitivity" "99.7")
  (-main "--edges" "false")
  (-main "--edges" "true" "--edges-cut-off" "2")
  (-main "--edges" "true" "--edges-cut-off" "3")
  .)

;;

(comment
  (require '[criterium.core :refer [bench quick-bench]])

  (quick-bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 36 in 6 samples of 6 calls.
  ;;           Execution time mean : 21,216211 ms
  ;;  Execution time std-deviation : 3,674049 ms
  ;; Execution time lower quantile : 18,901281 ms ( 2,5%)
  ;; Execution time upper quantile : 27,425971 ms (97,5%)
  ;;                 Overhead used : 8,200265 ns
  (bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 3240 in 60 samples of 54 calls.
  ;;           Execution time mean : 18,749845 ms
  ;;  Execution time std-deviation : 421,709128 µs
  ;; Execution time lower quantile : 18,348777 ms ( 2,5%)
  ;; Execution time upper quantile : 19,815350 ms (97,5%)
  ;;                 Overhead used : 8,200265 ns
  .)
