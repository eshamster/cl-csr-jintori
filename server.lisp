(defpackage cl-csr-jintori/server
  (:use :cl
        :cl-markup)
  (:export :start-game
           :stop-game)
  (:import-from :cl-csr-2d-game
                :start
                :stop)
  (:import-from :cl-csr-jintori/process
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

(defvar *parent-entity* nil)

(defun start-game (&key (port 5000))
  (stop-game)
  (setf *parent-entity* (make-ecs-entity))
  (stack-default-ecs-entity-parent *parent-entity*)
  (start :port port
         :root-dir (asdf:component-pathname
                    (asdf:find-system :cl-csr-jintori))
         :init-func (lambda () (init-game))
         :update-func (lambda () (update-game))))

(defun stop-game ()
  (when *parent-entity*
    (let ((parent (pop-default-ecs-entity-parent)))
      (assert (eq *parent-entity* parent))
      (register-next-frame-func
       (lambda () (print
                   (let ((count 0))
                     (cl-ps-ecs::do-ecs-entities var
                       (incf count))
                     count))
               (delete-ecs-entity parent))))
    (setf *parent-entity* nil))
  (stop))

