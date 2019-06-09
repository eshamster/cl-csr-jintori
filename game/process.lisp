(defpackage cl-csr-jintori/game/process
  (:use :cl)
  (:export :update-game
           :init-game)
  (:import-from :cl-csr-jintori/game/state/package
                :init-cl-csr-jintori-state)
  (:import-from :cl-csr-2d-game
                :set-update-frequency))
(in-package :cl-csr-jintori/game/process)

(defun init-game ()
  (set-update-frequency 30 2)
  (init-cl-csr-jintori-state))

(defun update-game ())
