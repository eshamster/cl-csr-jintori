(defpackage cl-csr-jintori/game/each-client-manager
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :init-client-manager
           :update-client-manager)
  (:import-from :cl-csr-jintori/game/balloon
                :add-or-change-balloon)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param)
  (:import-from :cl-csr-jintori/game/touch-marker
                :add-touch-marker)
  (:import-from :cl-csr-jintori/game/utils
                :out-of-screen-p)
  (:import-from :cl-csr
                :get-client-id-list
                :get-new-client-id-list
                :get-deleted-client-id-list
                :mouse-down-now-p
                :get-mouse-pos
                :get-touch-summary-pos
                :touch-summary-down-now-p)
  (:import-from :alexandria
                :appendf))
(in-package :cl-csr-jintori/game/each-client-manager)

;; --- interface --- ;;

(defun init-client-manager ()
  (dolist (id (get-client-id-list))
    (add-each-client-manager id))
  (init-color-table))

(defun update-client-manager ()
  (dolist (id (get-new-client-id-list))
    (add-each-client-manager id))
  (dolist (id (get-deleted-client-id-list))
    (delete-each-client-manager id))
  (update-color-table))

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
                            :color (get-new-color client-id)))
       (add-ecs-entity manager)))))

(defun process-client-manager (manager-entity)
  (let ((id (get-entity-param manager-entity :client-id))
        (color (get-entity-param manager-entity :color)))
    (when (or (mouse-down-now-p id :left)
              (touch-summary-down-now-p id))
      (multiple-value-bind (x y) (get-mouse-or-touch-pos id)
        (unless (out-of-screen-p x y 0)
          (let ((success-p (add-or-change-balloon id x y color)))
            (add-touch-marker :x x :y y :color color :client-id id
                              :success-p success-p)))))))

(defun delete-each-client-manager (client-id)
  (let ((manager (find-each-client-manager client-id)))
    (when manager
      (register-next-frame-func
       (when (find-the-entity manager)
         (lambda () (delete-ecs-entity manager)))))))

(defun find-each-client-manager (client-id)
  (let (result)
    (do-tagged-ecs-entities (manager :each-client-manager)
      (when (= client-id (get-entity-param manager :client-id))
        (setf result manager)
        (return)))
    result))

(defun get-mouse-or-touch-pos (client-id)
  ;; Note: Assume that it is called when some input is in donw-now state.
  (cond ((mouse-down-now-p client-id :left)
         (get-mouse-pos client-id))
        ((touch-summary-down-now-p client-id)
         (get-touch-summary-pos client-id))
        (t (error "No input is in donw-now state"))))

;; - color table - ;;

(defvar *reserved-color-queue* nil)
(defvar *used-color-table* (make-hash-table)
  "Key: client id, Value: color")

(defun init-color-table ()
  (setf *reserved-color-queue* (copy-list (get-param :client :color-table)))
  (setf *used-color-table* (make-hash-table)))

(defun queue-color (color)
  (appendf *reserved-color-queue* (list color)))

(defun dequeue-color ()
  (pop *reserved-color-queue*))

(defun update-color-table ()
  (dolist (client-id (get-deleted-client-id-list))
    (let ((color (gethash client-id *used-color-table*)))
      (when color
        (remhash client-id *used-color-table*)
        (queue-color color)))))

(defun get-new-color (client-id)
  (let ((color (dequeue-color)))
    (if color
        (setf (gethash client-id *used-color-table*) color)
        (let ((random-color 0))
          (dotimes (i 3)
            (setf random-color
                  (+ (ash random-color 8) (random #x100))))
          random-color))))
