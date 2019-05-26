(defpackage cl-csr-jintori/game/each-client-manager
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :init-client-manager)
  (:import-from :cl-csr-jintori/game/balloon
                :add-balloon)
  (:import-from :proto-cl-client-side-rendering
                :register-callback-on-connecting
                :register-callback-on-disconnecting
                :mouse-down-now-p
                :touch-summary-down-now-p))
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
                                (process-client-manager entity)))
        (init-entity-params :client-id client-id
                            :color (get-next-color)))
       (add-ecs-entity manager)))))

(defun process-client-manager (manager-entity)
  (let ((id (get-entity-param manager-entity :client-id))
        (color (get-entity-param manager-entity :color)))
    (when (or (mouse-down-now-p id :left)
              (touch-summary-down-now-p id))
      (add-balloon :client-id id :color color))))

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

