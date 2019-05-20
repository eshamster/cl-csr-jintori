(defpackage cl-csr-jintori/game/state/main
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:import-from :proto-cl-client-side-rendering
                :log-console))
(in-package :cl-csr-jintori/game/state/main)

(def-game-state main ((parent (make-ecs-entity)))
  :start-process
  (state-lambda (parent)
    (log-console :message "start-process")
    (stack-default-ecs-entity-parent parent)
    t)
  :process
  (state-lambda ()
    (log-console :message "process")
    nil)
  :end-process
  (state-lambda (parent)
    (log-console :message "end-process")
    (let ((got-parent (pop-default-ecs-entity-parent)))
      (assert (eq parent got-parent)))
    (delete-ecs-entity parent)
    t))
