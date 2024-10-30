(ns user)

;; NB: Optionally requiring snitch for a sane debugging experience.
;;     https://github.com/AbhinavOmprakash/snitch
(try (require '[snitch.core :refer [defn* defmethod* *fn *let]])
     (catch Exception _))

;; NB: Assign these to a dedicated project-specific REPL command.
(load "/space_invaders/domain")
(in-ns 'space-invaders.domain)
