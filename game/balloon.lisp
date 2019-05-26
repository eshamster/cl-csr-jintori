(defpackage cl-csr-jintori/game/balloon
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-balloon
           :find-collided-balloon
           :try-changing-balloon-owner)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param)
  (:import-from :proto-cl-client-side-rendering
                :mouse-up-p
                :mouse-down-now-p
                :touch-summary-up-p
                :touch-summary-down-now-p))
(in-package :cl-csr-jintori/game/balloon)

(defmacro get-balloon-param (&rest keys)
  `(get-param :balloon ,@keys))

(defmacro do-balloon ((var) &body body)
  `(do-tagged-ecs-entities (,var :balloon)
     ,@body))

;; --- interface --- ;;

(defun add-balloon (&key x y client-id color)
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
    (add-ecs-entity balloon)))

(defun try-changing-balloon-owner (&key balloon client-id color)
  (check-entity-tags balloon :balloon)
  (when (has-entity-tag balloon :balloon-fragile)
    (delete-entity-tag balloon :balloon-fragile)
    (set-entity-param balloon
                      :client-id client-id
                      :color color)))

(defun find-collided-balloon (&key x y r)
  (do-balloon (balloon)
    (let ((balloon-pos (calc-global-point balloon))
          (balloon-r (get-entity-param balloon :r)))
      (when (<= (calc-dist-p2 balloon-pos
                              (make-vector-2d :x x :y y))
                (expt (+ r balloon-r) 2))
        (return-from find-collided-balloon balloon)))))

;; --- internal --- ;;

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
  (let* ((can-expand-p (expand-balloon balloon (get-balloon-param :expand-speed)))
         (id (get-entity-param balloon :client-id))
         (up-p (and (mouse-up-p id :left) (touch-summary-up-p id))))
    (when (or (not can-expand-p) up-p)
      (make-state-guard :balloon balloon))))

;; TODO: Implement animation
(defstruct (state-guard
             (:include balloon-state
                       (process (state-lambda (balloon rest-guard-time)
                                  (decf rest-guard-time)
                                  (when (<= rest-guard-time 0)
                                    (make-state-fragile :balloon balloon))))))
  (rest-guard-time (get-balloon-param :guard-time)))

;; TODO: Implement changing owner if another client touch
(defstruct (state-fragile
             (:include balloon-state
                       (start-process (state-lambda (balloon)
                                        (add-entity-tag balloon :balloon-fragile)
                                        t))
                       (process (state-lambda (balloon)
                                  (unless (has-entity-tag balloon :balloon-fragile)
                                    (expand-balloon balloon 0)
                                    (make-state-guard :balloon balloon)))))))
