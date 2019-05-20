(defpackage cl-csr-jintori/game/process
  (:use :cl)
  (:export :update-game
           :init-game)
  (:import-from :cl-csr-jintori/game/state/package
                :init-cl-csr-jintori-state))
(in-package :cl-csr-jintori/game/process)

(defun init-game ()
  (init-cl-csr-jintori-state))

(defun update-game ())
