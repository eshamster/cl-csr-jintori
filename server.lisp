(defpackage cl-csr-jintori/server
  (:use :cl
        :cl-markup)
  (:export :start-game
           :stop-game)
  (:import-from :cl-csr-2d-game
                :start
                :stop)
  (:import-from :cl-csr-jintori/game/process
                :init-game
                :update-game)
  (:import-from :proto-cl-client-side-rendering
                :ensure-js-files
                :make-src-list-for-script-tag
                :make-client-side-rendering-middleware)
  (:import-from :cl-ps-ecs
                :make-ecs-entity
                :delete-ecs-entity
                :stack-default-ecs-entity-parent
                :pop-default-ecs-entity-parent
                :register-next-frame-func))
(in-package :cl-csr-jintori/server)

(defvar *port* 5000)

(defun start-game (&key (port *port*))
  (stop-game)
  (start :port port
         :root-dir (asdf:component-pathname
                    (asdf:find-system :cl-csr-jintori))
         :init-func (lambda () (init-game))
         :update-func (lambda () (update-game)))
  (setf *port* port))

(defun stop-game ()
  (stop))

