(ns space-invaders.cli
  "CLI entrypoint"
  (:require [clansi.core :refer [style]]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [failjure.core :as f]
            [io.aviso.exception :as ex]
            [space-invaders.domain :as d]
            [space-invaders.printing :as p]))

(def cli-options-spec
  [["-i" "--invader PATH"
    "Path to a text file with an invader pattern to search for on the radar.
    If none of these are specified, known invader patterns will be used."
    :multi true
    :default []
    :update-fn conj]
   ["-s" "--sensitivity SENSITIVITY"
    "Search sensitivity in percent in range 0 (exclusive) — 100 (inclusive)."
    :default 80.0
    :parse-fn #(Float/parseFloat %)
    :validate [#(and (float? %) (pos? %) (<= % 100))
               "Must be a floating point number in the range (0 .. 100]."]]
   ["-e" "--edges ON"
    "Turns off/on the search along edges of the input string (radar sample)."
    :default true
    :parse-fn #(Boolean/parseBoolean %)]
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
             "  clojure -M:run --edges false --sensitivity 70.0 path/to/radar_sample.txt"
             "  clojure -M:run --edges-cut-off 2 path/to/radar_sample.txt"
             ]))

(defn- args-error-msg [errors]
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
      {:radar-sample-path (first arguments)
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
      (f/try-all [invaders        (d/build-invaders (:invader options))
                  radar-sample    (d/build-radar-sample radar-sample-path invaders)
                  matches-by-type (d/find-invaders invaders radar-sample options)]
        (let [results {:invaders        invaders
                       :radar-sample    radar-sample
                       :matches-by-type matches-by-type}]
          (p/print-matches-as-list results)
          (p/print-matches-on-radar-sample results)
          (exit {:status-code 0
                 :output-msg  (when (seq matches-by-type)
                                "Show this to the commander, quickly!")}))
        (f/when-failed [e]
          (exit {:status-code 2
                 :output-msg  (format "Something went wrong:\n%s"
                                      (if (instance? Throwable e)
                                        (ex/format-exception e)
                                        (style (f/message e) :red)))}))))))

(comment
  (-main)
  (-main "--edges-cut-off" "2")
  (-main "--edges-cut-off" "3")
  (-main "--edges" "false")
  (-main "--edges" "false" "--sensitivity" "70.0")
  (-main "-h")
  .)
