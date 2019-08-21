(defpackage cl-csr-jintori/game/touch-marker
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-touch-marker)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param
                :get-depth)
  (:import-from :proto-cl-client-side-rendering
                :mouse-down-now-p
                :touch-summary-down-now-p))
(in-package :cl-csr-jintori/game/touch-marker)

(defmacro get-marker-param (&rest keys)
  `(get-param :marker ,@keys))

;; --- interface --- ;;

(defun add-touch-marker (&key x y color client-id)
  (add-global-touch-marker :x x :y y :color color)
  (add-local-touch-marker :x x :y y :client-id client-id))

;; --- internal --- ;;

;; TODO: Slightly make color stand out.
(defun add-global-touch-marker (&key x y color)
  (let ((marker (make-ecs-entity))
        (rest-duration (get-marker-param :global :duration)))
    (add-ecs-component-list
     marker
     (make-point-2d :x x :y y)
     (make-model-2d :mesh (make-circle-mesh :color color
                                            :r (get-marker-param :global :r)
                                            :fill-p t)
                    :depth (get-depth :marker))
     (make-script-2d :func (lambda (entity)
                             (decf rest-duration)
                             (when (<= rest-duration 0)
                               (register-next-frame-func
                                (when (find-the-entity entity)
                                  (lambda () (delete-ecs-entity entity))))))))
    (add-ecs-entity marker)))

(defun add-local-touch-marker (&key x y client-id)
  (let ((marker (make-ecs-entity))
        (rest-duration (get-marker-param :local :duration))
        (half-len (/ (get-marker-param :local :length) 2)))
    (flet ((make-line-model (x1 y1 x2 y2)
             (make-model-2d :mesh (make-line-mesh :x1 x1 :y1 y1
                                                  :x2 x2 :y2 y2
                                                  :color #xff0000)
                            :depth (get-depth :marker)
                            :target-client-id-list (list client-id))))
      (add-ecs-component-list
       marker
       (make-point-2d :x x :y y)
       (make-line-model 0 (* -1 half-len) 0 half-len)
       (make-line-model (* -1 half-len) 0 half-len 0)
       (make-script-2d :func (lambda (entity)
                               (decf rest-duration)
                               (when (or (<= rest-duration 0)
                                         (mouse-down-now-p client-id :left)
                                         (touch-summary-down-now-p client-id))
                                 (register-next-frame-func
                                  (when (find-the-entity entity)
                                    (lambda () (delete-ecs-entity entity)))))))))
    (add-ecs-entity marker)))
