(defpackage cl-csr-jintori/game/state/package
  (:use :cl
        ;; The followings are required to make package-inferred-system to recognize them
        :cl-csr-jintori/game/state/global-init
        :cl-csr-jintori/game/state/main)
  (:export :init-cl-csr-jintori-state)
  (:import-from :cl-csr-2d-game
                :init-game-state
                :interrupt-game-state
                :make-state))
(in-package :cl-csr-jintori/game/state/package)

(defvar *initialized-p* nil)

(defun init-cl-csr-jintori-state ()
  (if *initialized-p*
      (interrupt-game-state (make-state :main))
      (progn (init-game-state (make-state :global-init))
             (setf *initialized-p* t))))
