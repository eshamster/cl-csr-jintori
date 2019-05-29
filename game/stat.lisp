(defpackage cl-csr-jintori/game/stat
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :init-client-stat-manager)
  (:import-from :cl-csr-jintori/game/balloon
                :do-balloon
                :get-balloon-space
                :get-balloon-color
                :get-balloon-client-id)
  (:import-from :cl-csr-jintori/game/parameter
                :get-depth
                :get-param)
  (:import-from :alexandria
                :ensure-gethash
                :hash-table-keys))
(in-package :cl-csr-jintori/game/stat)

(defmacro get-stat-param (&rest keys)
  `(get-param :stat-graph ,@keys))

;; --- interface --- ;;

(defun init-client-stat-manager ()
  (let ((manager (make-ecs-entity)))
    (add-entity-tag manager :stat-manager)
    (add-ecs-component-list
     manager
     (make-point-2d :x (get-stat-param :x)
                    :y (get-stat-param :y))
     (make-script-2d :func (lambda (entity)
                             (register-next-frame-func
                              (lambda () (add-or-replace-stat-graph entity))))))
    (add-ecs-entity manager)))

;; --- internal --- ;;

(defstruct client-stat color (total-space 0))

(defun get-total-space ()
  (* #lx1000 #ly1000))

(defun add-or-replace-stat-graph (stat-manager)
  (delete-ecs-component-type 'model-2d stat-manager)
  (add-ecs-component (make-background-model) stat-manager)
  (let ((stat-table (create-client-stat-table))
        (x 0)
        (max-length (get-stat-param :length))
        (height (get-stat-param :width))
        (total-space (get-total-space)))
    (dolist (client-id (sort (hash-table-keys stat-table) #'<))
      (let* ((client-stat (gethash client-id stat-table))
             (length (* max-length (/ (client-stat-total-space client-stat)
                                      total-space))))
        (add-ecs-component
         (make-model-2d :mesh (make-rect-mesh :width length
                                              :height height
                                              :color (client-stat-color client-stat)
                                              :fill-p t)
                        :offset (make-point-2d :x x)
                        :depth (get-depth :stat :foreground))
         stat-manager)
        (incf x length)))))

(defun make-background-model ()
  (make-model-2d :mesh (make-rect-mesh
                        :color #x444444
                        :width (get-stat-param :length)
                        :height (get-stat-param :width)
                        :fill-p t)
                 :depth (get-depth :stat :background)))

(defun create-client-stat-table ()
  (let ((result (make-hash-table)))
    (do-balloon (balloon)
      (let ((stat (ensure-gethash
                   (get-balloon-client-id balloon)
                   result
                   (make-client-stat
                    :color (get-balloon-color balloon)))))
        (incf (client-stat-total-space stat)
              (get-balloon-space balloon))))
    result))
