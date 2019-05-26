(defpackage cl-csr-jintori/game/balloon
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-balloon)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param)
  (:import-from :proto-cl-client-side-rendering
                :get-mouse-pos
                :mouse-up-p))
(in-package :cl-csr-jintori/game/balloon)

;; --- interface --- ;;

(defun add-balloon (&key client-id color)
  ;; TODO: Get touch pos if touched.
  (multiple-value-bind (x y) (get-mouse-pos client-id)
    ;; TODO: Check if the pointed place is empty.
    (let ((balloon (make-ecs-entity))
          (r (get-balloon-param :first-r)))
      (add-entity-tag balloon :balloon)
      (add-ecs-component-list
       balloon
       (make-point-2d :x x :y y)
       (make-balloon-model r color)
       (make-script-2d :func (lambda (entity)
                               (process-game-state
                                (get-entity-param entity :state-manager))))
       (init-entity-params
        :state-manager (init-game-state-manager
                        (make-state-expand :balloon balloon))
        :client-id client-id
        :color color
        :r r))
      (add-ecs-entity balloon))))

;; --- internal --- ;;

(defmacro get-balloon-param (&rest keys)
  `(get-param :balloon ,@keys))

(defun make-balloon-model (r color)
  (make-model-2d :mesh (make-circle-mesh :r r :color color
                                         :fill-p t)))

(defun expand-balloon (balloon diff-r)
  (check-entity-tags balloon :balloon)
  (register-next-frame-func
   (lambda ()
     ;; TODO: Prevent sinking into another balloon
     (delete-ecs-component-type 'model-2d balloon)
     (let ((r (+ (get-entity-param balloon :r) diff-r)))
       (add-ecs-component-list
        balloon
        (make-balloon-model r (get-entity-param balloon :color)))
       (setf (get-entity-param balloon :r) r))))
  ;; TODO: Return nil when colliding to another balloon in the next frame.
  t)

;; - state - ;;

(defstruct (balloon-state (:include game-state))
  balloon)

(defstruct (state-expand
             (:include balloon-state
                       (process (state-lambda (balloon)
                                  (process-state-expand balloon))))))

(defun process-state-expand (balloon)
  (let ((can-expand-p (expand-balloon balloon (get-balloon-param :expand-speed)))
        ;; TODO: Consider touch input
        (up-p (mouse-up-p (get-entity-param balloon :client-id) :left)))
    (when (or (not can-expand-p) up-p)
      (make-state-guard :balloon balloon))))

;; TODO: Implement animation
(defstruct (state-guard
             (:include balloon-state
                       (process (state-lambda (balloon rest-guard-time)
                                  (incf rest-guard-time)
                                  (when (<= rest-guard-time 0)
                                    (make-state-fragile :balloon balloon))))))
  (rest-guard-time (get-balloon-param :guard-time)))

;; TODO: Implement changing owner if another client touch
(defstruct (state-fragile
             (:include balloon-state
                       )))
