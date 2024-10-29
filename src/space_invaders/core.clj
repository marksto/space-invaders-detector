(ns space-invaders.core
  (:require [babashka.fs :as fs]
            [clansi.core :refer [style]]
            [clj-fuzzy.metrics :as fuzzy-metrics]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [fipp.edn :refer [pprint]]))

;; text manipulations

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

;; patterns

(def valid-pattern-chars #{\- \o})

(defn validate-pattern-str [pattern-str]
  (let [pattern-lines (str/split-lines pattern-str)]
    (cond
      (seq (mapcat #(remove valid-pattern-chars %) pattern-lines))
      {:error/msg  "Pattern must contain only valid characters"
       :error/data {:pattern-lines pattern-lines}}
      ;;
      (not= 1 (count (set (map count pattern-lines))))
      {:error/msg  "Pattern lines have to be of the same length"
       :error/data {:pattern-lines pattern-lines}})))

(defn ->pattern [pattern-str]
  {:pattern/text      pattern-str
   :pattern/dims      (text-dimensions pattern-str)
   :pattern/char-seqs (text-str->char-seqs pattern-str)})

;; input strings

(defn validate-input-str
  [input-str [min-width min-height :as min-dims]]
  (let [input-lines (str/split-lines input-str)
        [input-width input-height :as input-dims] (text-dimensions input-str)]
    (cond
      (not= 1 (count (set (map count input-lines))))
      {:error/msg  "Input lines have to be of the same length"
       :error/data {:input-lines input-lines}}
      ;;
      (or (< input-width min-width)
          (< input-height min-height))
      {:error/msg  "Input dimension must be equal to or bigger than the minimal"
       :error/data {:input-dims input-dims
                    :min-dims   min-dims}})))

(defn ->input
  [input-str]
  {:input/text      input-str
   :input/dims      (text-dimensions input-str)
   :input/char-seqs (text-str->char-seqs input-str)})

;; pattern matching

(defn calc-distance
  [char-seq-1 char-seq-2]
  (assert (= (count char-seq-1) (count char-seq-2))
          "The Hamming distance is only defined for seqs of the same length")
  (fuzzy-metrics/hamming char-seq-1 char-seq-2))

(defn ->pattern-match
  [p-char-seq p-dims
   i-char-seqs i-loc
   min-accuracy
   & [partial-match? edge-kind]]
  (let [i-subseq (extract-char-subseq i-char-seqs i-loc p-dims)
        distance (calc-distance p-char-seq i-subseq)
        accuracy (- 100.0 (/ distance (count p-char-seq)))]
    (when (<= min-accuracy accuracy)
      (cond-> {:match/location  i-loc
               :match/distance  distance
               :match/accuracy  accuracy
               :match/char-seqs (partition (first p-dims) i-subseq)}
              (true? partial-match?) (assoc :match/partial? true)
              (some? edge-kind) (assoc :match/edge-kind edge-kind)))))

(defmulti edge-base-locs
  (fn [edge-kind _pattern _input]
    edge-kind))

(defmethod edge-base-locs :top
  [_
   {[p-width] :pattern/dims :as _pattern}
   {[i-width] :input/dims :as _input}]
  (map #(vector % 0)
       (range (inc (- i-width p-width)))))

(defmethod edge-base-locs :left
  [_
   {[_ p-height] :pattern/dims :as _pattern}
   {[_ i-height] :input/dims :as _input}]
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

(defmulti edge-shifts
  (fn [edge-kind _pattern _min-sub-pattern]
    edge-kind))

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
       (range 1)
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

(defn- find-edge-matches
  [edge-kind
   {p-char-seqs :pattern/char-seqs :as pattern}
   {i-char-seqs :input/char-seqs :as input}
   min-sub-pattern
   min-accuracy]
  (let [edge-base-locs (edge-base-locs edge-kind pattern input)
        edge-shifts    (edge-shifts edge-kind pattern min-sub-pattern)]
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
                                                      min-accuracy true edge-kind)]
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
   (let [pattern (->pattern pattern-str)
         input   (->input input-str)]
     (concat (find-full-matches pattern input min-accuracy)
             (when search-on-edges
               (find-matches-on-edges
                 pattern input min-sub-pattern min-accuracy))))))

;; main logic (high-level)

(declare print-validation-error)

(defn max-invader-dims
  [invaders]
  (reduce (fn [[max-width max-height] {:invader/keys [pattern]}]
            (let [[pattern-width pattern-height] (text-dimensions pattern)]
              [(max max-width pattern-width) (max max-height pattern-height)]))
          [1 1]
          invaders))

(defn- prepare-search-opts
  [{:keys [sensitivity edges edges-cut-off] :as _opts}]
  (cond-> {}
          (some? sensitivity) (assoc :min-accuracy sensitivity)
          (true? edges) (assoc :search-on-edges true)
          (some? edges-cut-off) (assoc :min-sub-pattern (max 1 edges-cut-off))))

(defn- find-invader
  [{:invader/keys [pattern] :as _invader} radar-sample opts]
  (if-some [error (validate-pattern-str pattern)]
    (do (print-validation-error error)
        nil)
    (find-matches pattern radar-sample opts)))

(defn find-invaders
  {:arglists '([invaders radar-sample]
               [invaders radar-sample {:keys [sensitivity edges edges-cut-off]
                                       :as   _opts}])}
  ([invaders radar-sample]
   (find-invaders invaders radar-sample nil))
  ([invaders radar-sample opts]
   (let [max-invader-dims (max-invader-dims invaders)]
     (if-some [error (validate-input-str radar-sample max-invader-dims)]
       (do (print-validation-error error)
           nil)
       (let [search-opts (prepare-search-opts opts)]
         (reduce (fn [res {invader-type :invader/type :as invader}]
                   (let [matches (find-invader invader radar-sample search-opts)]
                     (if (seq matches)
                       (assoc res invader-type matches)
                       res)))
                 {}
                 invaders))))))

;;

(def example-radar-sample-path
  "space_invaders/radar_samples/example.txt")

(def default-invader-pattern-paths
  ["space_invaders/invaders/squid.txt"
   "space_invaders/invaders/crab.txt"])

(declare read-text-file)

(defn build-invaders
  ([]
   (build-invaders nil))
  ([invader-pattern-paths]
   (reduce (fn [res pattern-path]
             (let [filename        (fs/strip-ext (fs/file-name pattern-path))
                   invader-pattern (read-text-file pattern-path)]
               (conj res {:invader/type    (keyword filename)
                          :invader/pattern invader-pattern})))
           []
           (or (seq invader-pattern-paths)
               default-invader-pattern-paths))))

;; I/O and entrypoint

(defn read-text-file [path]
  (some-> (or (io/resource path) (io/file path))
          (slurp)))

(defn print-validation-error
  [{:error/keys [msg data] :as _error}]
  (println (style msg :red))
  (println (style (with-out-str (pprint data)) :red)))

(defn- ->output-match
  [{:match/keys [location char-seqs distance accuracy partial? edge-kind]
    :as         _match}]
  (cond-> {:location location
           #_#_:distance distance ; for debug purposes
           :accuracy (format "%.2f%%" accuracy)
           :matching (mapv str/join char-seqs)}
          (true? partial?) (assoc :partial? true)
          (some? edge-kind) (assoc :edge-kind edge-kind)))

(def matches-comp
  "Compares matches first by the total length (the bigger the better)
   and then by `accuracy` of the match (again, the bigger the better)."
  (juxt #(- (reduce + (map count (:match/char-seqs %))))
        #(- (:match/accuracy %))))

(defn print-results [results]
  (when (some? results)
    (let [total-matches (reduce + 0 (map (comp count second) results))]
      (println (format "Found %s possible invader matches in total.\n"
                       total-matches)))
    (doseq [[invader-type matches] results]
      (println (format "Found %s possible '%s' invader matches:"
                       (count matches) (name invader-type)))
      (doseq [output-match (->> matches
                                (sort-by matches-comp)
                                (map ->output-match))]
        (pprint output-match))
      (println))))

(defn invader-type->color [invaders]
  (let [predefined-colors [:red :green :blue :yellow :magenta :cyan]
        invader-types     (distinct (map :invader/type invaders))]
    (when (< (count predefined-colors)
             (count invader-types))
      (println
        (style "Not enough predefined colors for all invader types" :yellow)))
    (zipmap invader-types (concat predefined-colors (repeat :black)))))

(defn- build-loc->match-char
  [results invader-type->color]
  (reduce-kv
    (fn [acc invader-type matches]
      (let [color (get invader-type->color invader-type)]
        (loop [acc acc, matches matches]
          (if (seq matches)
            (recur
              (let [{char-seqs :match/char-seqs
                     [mx my]   :match/location} (first matches)
                    idy+m-lines (map-indexed (fn [idy m-line] [idy m-line])
                                             char-seqs)]
                (reduce (fn [acc [idy m-line]]
                          (reduce (fn [acc idx]
                                    (assoc acc [(+ mx idx) (+ my idy)]
                                           (style (nth m-line idx) color)))
                                  acc
                                  (range (count m-line))))
                        acc
                        idy+m-lines))
              (rest matches))
            acc))))
    {}
    results))

(defn print-radar-sample-with-matches
  [invaders radar-sample results]
  (when (seq results)
    (println "Possible invaders on the radar sample:")
    (let [[width height] (text-dimensions radar-sample)
          char-seqs           (text-str->char-seqs radar-sample)
          invader-type->color (invader-type->color invaders)
          loc->match-char     (build-loc->match-char results invader-type->color)]
      (doseq [idy (range height)]
        (println (apply str (map (fn [idx]
                                   (or (get loc->match-char [idx idy])
                                       (nth (nth char-seqs idy) idx)))
                                 (range width))))))
    (println)))

(def cli-options-spec
  [["-i" "--invader PATH"
    "Path to a text file with an invader pattern to search for on the radar."
    :multi true
    :default []
    :update-fn conj]
   ["-s" "--sensitivity SENSITIVITY"
    "Search sensitivity in percent in range 0 (exclusive) — 100 (inclusive)."
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
   ["-h" "--help"
    "Shows this very help message."]])

(defn- show-help-msg [options-summary]
  (str/join \newline
            ["Takes invader patterns and a radar sample as arguments and reveals possible locations of those pesky invaders."
             ""
             "USAGE"
             "  clojure -M:run [option ...] [radar_sample]"
             ""
             "ARGUMENTS"
             "  radar_sample  A path to a text file with the radar sample."
             "                If not specified, an example sample is used."
             ""
             "OPTIONS"
             options-summary
             ""
             "EXAMPLES"
             "  clojure -M:run"
             "  clojure -M:run path/to/radar_sample.txt"
             "  clojure -M:run -i path/to/invader_1.txt -i path/to/invader_2.txt"
             "  clojure -M:run --edges false path/to/radar_sample.txt"
             "  clojure -M:run --edges false --sensitivity 99.7 path/to/radar_sample.txt"
             "  clojure -M:run --edges-cut-off 2 path/to/radar_sample.txt"
             ]))

(defn args-error-msg [errors]
  (style (str "The following errors occurred while parsing your command:\n"
              (str/join \newline errors))
         :red))

(defn- validate-args
  "Validate command line arguments. Either return a map indicating the program
   should exit (with a single `:do-exit` key), or a map with input and options
   to the program."
  [args]
  (let [{:keys [options summary errors arguments]} (cli/parse-opts
                                                     args cli-options-spec)]
    (cond
      (:help options) ; => exit w/ help summary
      {:do-exit {:status-code 0
                 :output-msg  (show-help-msg summary)}}
      errors ; => exit w/ description of errors
      {:do-exit {:status-code 1
                 :output-msg  (args-error-msg errors)}}
      :else
      {:radar-sample-path (or (first arguments) example-radar-sample-path)
       :options           options})))

(defn- exit
  [{:keys [status-code output-msg]}]
  (when output-msg
    (println output-msg))
  (when-not *repl*
    (System/exit status-code)))

(defn -main [& args]
  (let [{:keys [do-exit radar-sample-path options]} (validate-args args)]
    (if do-exit
      (exit do-exit)
      (try
        (let [radar-sample (read-text-file radar-sample-path)
              invaders     (build-invaders (:invader options))
              results      (find-invaders invaders radar-sample options)]
          (print-results results)
          (print-radar-sample-with-matches invaders radar-sample results)
          (exit {:status-code 0
                 :output-msg  (when (seq results)
                                "Show this to the commander, quickly!")}))
        (catch Exception ex
          (exit {:status-code 2
                 :output-msg  (style (format "Something went wrong:\n%s" ex)
                                     :red)}))))))

(comment
  (-main)
  (-main "--edges-cut-off" "2")
  (-main "--edges-cut-off" "3")
  (-main "--edges" "false")
  (-main "--edges" "false" "--sensitivity" "99.725")
  (-main "-h")
  .)

;;

(comment
  (require '[criterium.core :refer [bench quick-bench]])

  (quick-bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 36 in 6 samples of 6 calls.
  ;;           Execution time mean : 22,030480 ms
  ;;  Execution time std-deviation : 3,460200 ms
  ;; Execution time lower quantile : 19,739008 ms ( 2,5%)
  ;; Execution time upper quantile : 27,528698 ms (97,5%)
  ;;                 Overhead used : 8,305613 ns
  (bench (find-invaders invaders radar-sample))
  ;;Evaluation count : 3060 in 60 samples of 51 calls.
  ;;           Execution time mean : 20,032871 ms
  ;;  Execution time std-deviation : 527,714636 µs
  ;; Execution time lower quantile : 19,607813 ms ( 2,5%)
  ;; Execution time upper quantile : 21,538275 ms (97,5%)
  ;;                 Overhead used : 8,222227 ns
  .)
