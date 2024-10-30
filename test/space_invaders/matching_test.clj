(ns space-invaders.matching-test
  (:require [expectations.clojure.test
             :refer [defexpect expect expecting more-of]]
            [space-invaders.matching :as sut]
            [space-invaders.text :as t]))

(defexpect validate-pattern-str-test
  (expecting "Valid pattern is returned as is"
    (doseq [valid-pattern [(t/str-vec->text-str ["---oo---"
                                                 "--oooo--"
                                                 "-oooooo-"
                                                 "oo-oo-oo"
                                                 "oooooooo"
                                                 "--o--o--"
                                                 "-o-oo-o-"
                                                 "o-o--o-o"])
                           (t/str-vec->text-str ["--o-----o--"
                                                 "---o---o---"
                                                 "--ooooooo--"
                                                 "-oo-ooo-oo-"
                                                 "ooooooooooo"
                                                 "o-ooooooo-o"
                                                 "o-o-----o-o"
                                                 "---oo-oo---"])]]
      (expect valid-pattern (sut/validate-pattern-str valid-pattern))))
  (expecting "Pattern must contain only valid characters"
    (expect (more-of {msg :message}
                     (expect #"^Pattern must contain only valid characters" msg))
            (sut/validate-pattern-str
              (t/str-vec->text-str ["--o-----o--"
                                    "---o---x---"
                                    "--ooooooo--"
                                    "-oo-ooo-oo-"
                                    "ooooooooooo"
                                    "o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"]))))
  (expecting "Pattern lines have to be of the same length"
    (expect (more-of {msg :message}
                     (expect #"^Pattern lines have to be of the same length" msg))
            (sut/validate-pattern-str
              (t/str-vec->text-str ["--o-----o--"
                                    "---o---o-"
                                    "--ooooooo--"
                                    "-oo-ooo-oo-"
                                    "ooooooooooo"
                                    "o-ooooooo-o"
                                    "o-o-----o-o"
                                    "---oo-oo---"])))))

(defexpect validate-input-str-test
  (let [max-invader-dims [11 8]]
    (expecting "Valid input string is returned as is"
      (doseq [valid-radar-sample [;; min size
                                  (t/str-vec->text-str ["-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"])
                                  ;; bigger width
                                  (t/str-vec->text-str ["-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"
                                                        "-------------"])
                                  ;; bigger height
                                  (t/str-vec->text-str ["-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"
                                                        "-----------"])]]
        (expect valid-radar-sample
                (sut/validate-input-str valid-radar-sample max-invader-dims))))
    (expecting "Input lines have to be of the same length"
      (expect (more-of {msg :message}
                       (expect #"^Input lines have to be of the same length" msg))
              (sut/validate-input-str
                (t/str-vec->text-str ["-----------"
                                      "----------"
                                      "-----------"
                                      "-----------"
                                      "---------"
                                      "-----------"
                                      "-----------"
                                      "-----------"])
                max-invader-dims)))
    (expecting "Input dimensions have to fit all patterns"
      (expect (more-of {msg :message}
                       (expect #"^Input dimensions have to fit all patterns" msg))
              (sut/validate-input-str
                (t/str-vec->text-str ["--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"
                                      "--------"])
                max-invader-dims)))))

;;

(comment
  (expectations.clojure.test/run-tests)
  .)
