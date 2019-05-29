(defpackage cl-csr-jintori/game/touch-marker
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-touch-marker)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param
                :get-depth))
(in-package :cl-csr-jintori/game/touch-marker)

(defmacro get-marker-param (&rest keys)
  `(get-param :marker ,@keys))

;; --- interface --- ;;

;; TODO: Slightly make color stand out.
(defun add-touch-marker (&key x y color)
  (let ((marker (make-ecs-entity))
        (rest-duration (get-marker-param :duration)))
    (add-ecs-component-list
     marker
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-circle-mesh :color color
                                            :r (get-marker-param :r)
                                            :fill-p t)
                    :depth (get-depth :marker))
     (make-script-2d :func (lambda (entity)
                             (decf rest-duration)
                             (when (<= rest-duration)
                               (register-next-frame-func
                                (when (find-the-entity entity)
                                  (lambda () (delete-ecs-entity entity))))))))
    (add-ecs-entity marker)))

;; --- internal --- ;;

