(ns space-invaders.core-test
  (:require [expectations.clojure.test
             :refer [approximately defexpect expect expecting more-> more-of]]
            [space-invaders.core :as sut]))

(defn ≈ [^double v]
  (approximately v 0.01))

(defn char-seq [str-vec]
  (mapcat seq str-vec))

;;

(defexpect find-invaders-test
  (expecting "Happy path works at 99.8% sensitivity — task test data"
    (expect (more->
              #{:invader.type/squid :invader.type/crab} (-> keys set)

              (more-of [s1 s2 s3 s4 :as squids]
                       (expect 4 (count squids))
                       (expect (more-> [42 0] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seq ["---oo---"
                                                  "--ooo-o-"
                                                  "--ooooo-"
                                                  "oo--o-oo"
                                                  "oo-ooooo"
                                                  "-----o--"
                                                  "oo-oo-o-"
                                                  "o-o--ooo"]) :match/char-seq)
                               s1)
                       (expect (more-> [35 15] :match/location
                                       (≈ 99.84) :match/accuracy
                                       (char-seq ["---oo---"
                                                  "--oooo--"
                                                  "oooooooo"
                                                  "oo-oo--o"
                                                  "-ooooooo"
                                                  "--o--ooo"
                                                  "-o-oo---"
                                                  "oo--oo-o"]) :match/char-seq)
                               s2)
                       (expect (more-> [16 28] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seq ["---o-o--"
                                                  "-ooooo--"
                                                  "-oooooo-"
                                                  "o--oo-oo"
                                                  "oooooooo"
                                                  "-ooo-o--"
                                                  "--ooo-o-"
                                                  "o-o--ooo"]) :match/char-seq)
                               s3)
                       (expect (more-> [82 41] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seq ["---oo---"
                                                  "--ooooo-"
                                                  "-oo-ooo-"
                                                  "oo-o-ooo"
                                                  "o-oooooo"
                                                  "--o--o--"
                                                  "oo-oo-o-"
                                                  "--oooo-o"]) :match/char-seq)
                               s4))
              :invader.type/squid

              (more-of [c1 c2 c3 :as crabs]
                       (expect 3 (count crabs))
                       (expect (more-> [74 1] :match/location
                                       (≈ 99.88) :match/accuracy
                                       (char-seq ["ooo-----o--"
                                                  "o--o-o-o---"
                                                  "--o-ooooo--"
                                                  "oo--ooo-oo-"
                                                  "ooooooo-ooo"
                                                  "oooo--ooo-o"
                                                  "o-o-----o-o"
                                                  "---oo-oo---"]) :match/char-seq)
                               c1)
                       (expect (more-> [85 12] :match/location
                                       (≈ 99.86) :match/accuracy
                                       (char-seq ["--oo----o--"
                                                  "-------o---"
                                                  "o--oooooo--"
                                                  "-oo--oo--o-"
                                                  "oo-oooooooo"
                                                  "o-ooooooo-o"
                                                  "oo-o----o-o"
                                                  "--ooo-oo--o"]) :match/char-seq)
                               c2)
                       (expect (more-> [60 13] :match/location
                                       (≈ 99.91) :match/accuracy
                                       (char-seq ["--o-----o--"
                                                  "-------o---"
                                                  "--oooo-oo--"
                                                  "----ooo-oo-"
                                                  "o--oooooo-o"
                                                  "o-o-ooooo-o"
                                                  "o-o-----o-o"
                                                  "---oo-oo---"]) :match/char-seq)
                               c3))
              :invader.type/crab)

            (sut/find-invaders sut/invaders sut/radar-sample))))

(comment
  (expectations.clojure.test/run-tests)
  .)
