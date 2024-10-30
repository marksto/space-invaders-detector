(ns space-invaders.domain
  "Core domain logic (high level) for searching invaders on a radar sample"
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [failjure.core :as f]
            [space-invaders.matching :as m]
            [space-invaders.text :as t]))

(def example-radar-sample-path
  "space_invaders/radar_samples/example.txt")

(def default-invader-pattern-paths
  ["space_invaders/invaders/squid.txt"
   "space_invaders/invaders/crab.txt"])

(defn- read-text-file [path]
  (some-> (or (io/resource path) (io/file path))
          (slurp)))

;;

(def valid-invader-pattern-chars #{\- \o})

(defn build-invaders
  ([]
   (build-invaders nil))
  ([invader-pattern-paths]
   (reduce
     (fn [res pattern-path]
       (let [filename    (fs/strip-ext (fs/file-name pattern-path))
             pattern-str (m/validate-pattern-str (read-text-file pattern-path)
                                                 valid-invader-pattern-chars)]
         (if (f/failed? pattern-str)
           (reduced pattern-str)
           (conj res {:invader/type    (keyword filename)
                      :invader/pattern pattern-str}))))
     []
     (or (seq invader-pattern-paths)
         default-invader-pattern-paths))))

(defn- max-invader-dims
  [invaders]
  (reduce (fn [[max-width max-height] {:invader/keys [pattern]}]
            (let [[pattern-width pattern-height] (t/text-dimensions pattern)]
              [(max max-width pattern-width) (max max-height pattern-height)]))
          [1 1]
          invaders))

(defn build-radar-sample
  ([invaders]
   (build-radar-sample nil invaders))
  ([radar-sample-path invaders]
   (let [radar-sample-str (read-text-file (or radar-sample-path
                                              example-radar-sample-path))
         max-invader-dims (max-invader-dims invaders)]
     (m/validate-input-str radar-sample-str max-invader-dims))))

;;

(defn- prepare-search-opts
  [{:keys [sensitivity edges edges-cut-off] :as _opts}]
  (cond-> {}
          (some? sensitivity) (assoc :min-accuracy sensitivity)
          (true? edges) (assoc :search-on-edges true)
          (some? edges-cut-off) (assoc :min-sub-pattern (max 1 edges-cut-off))))

(defn find-invaders
  {:arglists '([invaders radar-sample]
               [invaders radar-sample {:keys [sensitivity edges edges-cut-off]
                                       :as   _opts}])}
  ([invaders radar-sample]
   (find-invaders invaders radar-sample nil))
  ([invaders radar-sample opts]
   (let [search-opts (prepare-search-opts opts)]
     (reduce (fn [res {invader-type    :invader/type
                       invader-pattern :invader/pattern :as _invader}]
               (let [matches (m/find-matches invader-pattern radar-sample search-opts)]
                 (if (seq matches)
                   (assoc res invader-type matches)
                   res)))
             {}
             invaders))))

;;

(comment
  (do (require '[criterium.core :refer [bench quick-bench]])
      (def known-invaders (build-invaders))
      (def example-radar-sample (build-radar-sample example-radar-sample-path
                                                    known-invaders))
      (def default-opts {:sensitivity 80.0}))
  (quick-bench
    (find-invaders known-invaders example-radar-sample default-opts))
  ;;Evaluation count : 36 in 6 samples of 6 calls.
  ;;           Execution time mean : 21,286435 ms
  ;;  Execution time std-deviation : 3,965614 ms
  ;; Execution time lower quantile : 18,450143 ms ( 2,5%)
  ;; Execution time upper quantile : 28,043152 ms (97,5%)
  ;;                 Overhead used : 7,569705 ns
  (bench
    (find-invaders known-invaders example-radar-sample default-opts))
  ;;Evaluation count : 3420 in 60 samples of 57 calls.
  ;;           Execution time mean : 17,976447 ms
  ;;  Execution time std-deviation : 406,514836 µs
  ;; Execution time lower quantile : 17,563975 ms ( 2,5%)
  ;; Execution time upper quantile : 18,877320 ms (97,5%)
  ;;                 Overhead used : 7,569705 ns
  .)
