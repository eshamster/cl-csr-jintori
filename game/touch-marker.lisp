(defpackage cl-csr-jintori/game/touch-marker
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-touch-marker)
  (:import-from :cl-csr-jintori/game/balloon
                :get-balloon-first-r)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param
                :get-depth)
  (:import-from :cl-csr
                :mouse-down-now-p
                :touch-summary-down-now-p))
(in-package :cl-csr-jintori/game/touch-marker)

(defmacro get-marker-param (&rest keys)
  `(get-param :marker ,@keys))

;; --- interface --- ;;

(defun add-touch-marker (&key x y color client-id success-p)
  (add-global-touch-marker :x x :y y :color color)
  (if success-p
      (add-local-successed-touch-marker :x x :y y :client-id client-id)
      (add-local-failed-touch-marker :x x :y y :client-id client-id)))

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

(defun make-touch-model-part (client-id mesh)
  (make-model-2d :mesh mesh
                 :depth (get-depth :marker)
                 :target-client-id-list (list client-id)))

(defun make-a-touch-line-model (client-id x1 y1 x2 y2)
  (make-touch-model-part
   client-id (make-line-mesh :x1 x1 :y1 y1
                             :x2 x2 :y2 y2
                             :color (get-marker-param :local :color))))

(defun make-local-touch-marker-base (&key x y client-id)
  "Make touch maker without model"
  (let ((marker (make-ecs-entity))
        (rest-duration (get-marker-param :local :duration)))
    (add-ecs-component-list
     marker
     (make-point-2d :x x :y y)
     (make-script-2d :func (lambda (entity)
                             (decf rest-duration)
                             (when (or (<= rest-duration 0)
                                       (mouse-down-now-p client-id :left)
                                       (touch-summary-down-now-p client-id))
                               (register-next-frame-func
                                (when (find-the-entity entity)
                                  (lambda () (delete-ecs-entity entity))))))))
    marker))

(defun add-local-successed-touch-marker (&key x y client-id)
  "Used when creating or changing balloon is successed."
  (let ((marker (make-local-touch-marker-base :x x :y y :client-id client-id))
        (half-len (/ (get-marker-param :local :length) 2)))
    (add-ecs-component-list
     marker
     (make-a-touch-line-model client-id 0 (* -1 half-len) 0 half-len)
     (make-a-touch-line-model client-id (* -1 half-len) 0 half-len 0))
    (add-ecs-entity marker)))

(defun add-local-failed-touch-marker (&key x y client-id)
  "Used when creating balloon is failed."
  (let ((marker (make-local-touch-marker-base :x x :y y :client-id client-id))
        (half-len (/ (get-marker-param :local :length) 2))
        (r (get-balloon-first-r)))
    (add-ecs-component-list
     marker
     (make-a-touch-line-model client-id
                              (* -1 half-len) (* -1 half-len)
                              (*  1 half-len) (*  1 half-len))
     (make-a-touch-line-model client-id
                              (* -1 half-len) (*  1 half-len)
                              (*  1 half-len) (* -1 half-len))
     (make-touch-model-part
      client-id (make-circle-mesh :color (get-marker-param :local :color)
                                  :r r)))
    (add-ecs-entity marker)))
