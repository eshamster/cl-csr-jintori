(defpackage cl-csr-jintori/game/state/global-init
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game))
(in-package :cl-csr-jintori/game/state/global-init)

(def-game-state global-init ()
  :start-process
  (state-lambda ()
    t)
  :process
  (state-lambda ()
    (make-state :main))
  :end-process
  (state-lambda ()
    t))
