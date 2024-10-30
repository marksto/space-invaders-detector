(ns space-invaders.test-utils)

(defn ≈
  "Given a value and an optional delta (default 0.01), return a predicate
   that expects its argument to be within that delta of the given value.

   NB: Differs from the Expectations pred version in two ways:
       - the default delta is one order of magnitude smaller
       - the argument of the resulting pred is `nil`-checked"
  ([^double v]
   (≈ v 0.01))
  ([^double v ^double d]
   (fn [x]
     (when (some? x)
       (<= (- v (Math/abs d)) x (+ v (Math/abs d)))))))
