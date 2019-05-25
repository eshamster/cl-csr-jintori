(defpackage cl-csr-jintori/game/each-client-manager
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :init-client-manager)
  (:import-from :proto-cl-client-side-rendering
                :register-callback-on-connecting
                :register-callback-on-disconnecting
                :log-console))
(in-package :cl-csr-jintori/game/each-client-manager)

;; --- interface --- ;;

(defun init-client-manager ()
  (register-callback-on-connecting
   'add-each-client-manager #'add-each-client-manager)
  (register-callback-on-disconnecting
   'delete-each-client-manager #'delete-each-client-manager))

;; --- internal --- ;;

(defun add-each-client-manager (client-id)
  (register-next-frame-func
   (lambda ()
     (let ((manager (make-ecs-entity)))
       (add-entity-tag manager :each-client-manager)
       (add-ecs-component-list
        manager
        (make-script-2d :func (lambda (entity)
                                (log-console
                                 :message
                                 (format nil "id=~D; color=0x~X"
                                         (get-entity-param entity :client-id)
                                         (get-entity-param entity :color)))))
        (init-entity-params :client-id client-id
                            :color (get-next-color)))
       (add-ecs-entity manager)))))

(defun delete-each-client-manager (client-id)
  (let ((manager (find-each-client-manager client-id)))
    (register-next-frame-func
     (lambda () (delete-ecs-entity manager)))))

(defun find-each-client-manager (client-id)
  (let (result)
    (do-tagged-ecs-entities (manager :each-client-manager)
      (when (= client-id (get-entity-param manager :client-id))
        (setf result manager)
        (return)))
    (assert result)
    result))

(defun get-next-color ()
  ;; TODO: Consider a better solution than simple random
  (let ((color 0))
    (dotimes (i 3)
      (setf color
            (+ (ash color 8) (random #x100))))
    color))

