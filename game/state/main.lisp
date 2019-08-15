(defpackage cl-csr-jintori/game/state/main
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:import-from :cl-csr-jintori/game/each-client-manager
                :init-client-manager
                :update-client-manager)
  (:import-from :cl-csr-jintori/game/stat
                :init-client-stat-manager))
(in-package :cl-csr-jintori/game/state/main)

(def-game-state main ((parent (make-ecs-entity)))
  :start-process
  (state-lambda (parent)
    (stack-default-ecs-entity-parent parent)
    (init-client-stat-manager)
    (init-client-manager)
    t)
  :process
  (state-lambda (parent)
    (update-client-manager)
    nil)
  :end-process
  (state-lambda (parent)
    (let ((popped-parent (pop-default-ecs-entity-parent)))
      (assert (eq popped-parent parent)))
    (delete-ecs-entity parent)
    t))
