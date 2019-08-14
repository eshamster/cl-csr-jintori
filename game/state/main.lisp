(defpackage cl-csr-jintori/game/state/main
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:import-from :cl-csr-jintori/game/each-client-manager
                :update-client-manager)
  (:import-from :cl-csr-jintori/game/stat
                :init-client-stat-manager))
(in-package :cl-csr-jintori/game/state/main)

(def-game-state main ((parent (make-ecs-entity)))
  :start-process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      (init-client-stat-manager))
    t)
  :process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      (update-client-manager))
    nil)
  :end-process
  (state-lambda (parent)
    (delete-ecs-entity parent)
    t))
