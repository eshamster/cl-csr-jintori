(defpackage cl-csr-jintori/game/balloon
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :add-or-change-balloon
           :do-balloon
           :get-balloon-r
           :get-balloon-space
           :get-balloon-color
           :get-balloon-client-id
           :get-balloon-first-r)
  (:import-from :cl-csr-jintori/game/parameter
                :get-depth
                :get-param)
  (:import-from :cl-csr-jintori/game/utils
                :out-of-screen-p)
  (:import-from :cl-csr
                :mouse-up-p
                :mouse-down-now-p
                :touch-summary-up-p
                :touch-summary-down-now-p
                :client-alive-p))
(in-package :cl-csr-jintori/game/balloon)

(defmacro get-balloon-param (&rest keys)
  `(get-param :balloon ,@keys))

;; --- interface --- ;;

(defmacro do-balloon ((var) &body body)
  `(do-tagged-ecs-entities (,var :balloon)
     ,@body))

(defun add-or-change-balloon (id x y color)
  "Add balloon or change balloon owner.
If can neither create nor change, returns nil.
Otherwise, returns generalized true."
  (let* ((r (get-balloon-param :first-r)))
    (when (out-of-screen-p x y r)
      (return-from add-or-change-balloon))
    (let ((balloon (find-collided-balloon :x x :y y :r 0)))
      (if balloon
          (try-changing-balloon-owner :balloon balloon
                                      :client-id id
                                      :color color)
          (unless (find-collided-balloon :x x :y y :r r)
            (add-balloon :client-id id :x x :y y :color color))))))

(defun get-balloon-r (balloon)
  (get-entity-param balloon :r))

(defun get-balloon-space (balloon)
  (* PI (expt (get-balloon-r balloon) 2)))

(defun get-balloon-color (balloon)
  (get-entity-param balloon :color))

(defun get-balloon-client-id (balloon)
  (get-entity-param balloon :client-id))

(defun get-balloon-first-r ()
  (get-balloon-param :first-r))

;; --- internal --- ;;

(defun add-balloon (&key x y client-id color)
  (let ((balloon (make-ecs-entity))
        (r (get-balloon-param :first-r)))
    (add-entity-tag balloon :balloon)
    (add-ecs-component-list
     balloon
     (make-point-2d :x x :y y)
     (make-balloon-model r color)
     (make-full-guard-model r)
     (make-script-2d :func (lambda (entity)
                             (process-game-state
                              (get-entity-param entity :state-manager))
                             (when (and (balloon-owned-p entity)
                                        (not (client-alive-p (get-entity-param entity :client-id))))
                               (try-changing-balloon-owner
                                :balloon entity
                                :client-id (get-param :nil-owner :id)
                                :color     (get-param :nil-owner :color)))))
     (init-entity-params
      :state-manager (init-game-state-manager
                      (make-state-expand :balloon balloon))
      :client-id client-id
      :color color
      :r r))
    (add-ecs-entity balloon)))

(defun balloon-owned-p (balloon)
  (not (= (get-entity-param balloon :client-id)
          (get-param :nil-owner :id))))

(defun try-changing-balloon-owner (&key balloon client-id color)
  (check-entity-tags balloon :balloon)
  (when (has-entity-tag balloon :balloon-fragile)
    (delete-entity-tag balloon :balloon-fragile)
    (set-entity-param balloon
                      :client-id client-id
                      :color color)
    t))

(defun find-collided-balloon (&key x y r)
  (do-balloon (balloon)
    (when (balloon-collided-p balloon x y r)
      (return-from find-collided-balloon balloon))))

(defun make-balloon-model (r color)
  (make-model-2d :mesh (make-circle-mesh :r r :color color
                                         :fill-p t)
                 :depth (get-depth :balloon)))

(defun make-rest-guard-model (r rest-duration max-duration)
  (make-model-2d :mesh (make-arc-mesh :r (* r 1.1)
                                      :color (get-balloon-param :edge-color)
                                      :start-angle (* -1/2 PI)
                                      :sweep-angle (* -2 PI (/ rest-duration
                                                               max-duration)))
                 :depth (get-depth :balloon-edge)
                 :label :balloon-edge))

(defun make-full-guard-model (r)
  (make-rest-guard-model r 1 1))

(defun update-model-in-guard-state (balloon rest-duration max-duration)
  (register-next-frame-func
   (lambda ()
     (delete-ecs-component (find-model-2d-by-label balloon :balloon-edge)
                           balloon)
     (when (> rest-duration 0)
       (add-ecs-component-list
        balloon
        (make-rest-guard-model (get-entity-param balloon :r)
                               rest-duration max-duration))))))

(defun expand-balloon (balloon diff-r)
  (check-entity-tags balloon :balloon)
  (register-next-frame-func
   (lambda ()
     ;; TODO: Prevent sinking into another balloon
     (delete-ecs-component-type 'model-2d balloon)
     (let ((r (+ (get-entity-param balloon :r) diff-r)))
       (add-ecs-component-list
        balloon
        (make-balloon-model r (get-entity-param balloon :color))
        (make-full-guard-model r))
       (setf (get-entity-param balloon :r) r))))
  (multiple-value-bind (x y r) (get-balloon-xyr balloon)
    (not (or (get-collided-balloon-list balloon x y (+ r diff-r))
             (out-of-screen-p x y (+ r diff-r))))))

(defun get-collided-balloon-list (balloon x y r)
  (let ((result nil))
    (do-balloon (target-balloon)
      (when (and (not (eq balloon target-balloon))
                 (balloon-collided-p target-balloon x y r))
        (push target-balloon result)))
    result))

(defun balloon-collided-p (balloon x y r)
  (let ((balloon-pos (calc-global-point balloon))
        (balloon-r (get-entity-param balloon :r)))
    (<= (calc-dist-p2 balloon-pos
                      (make-vector-2d :x x :y y))
        (expt (+ r balloon-r) 2))))

(defun get-balloon-xyr (balloon)
  (let ((pos (calc-global-point balloon))
        (r (get-balloon-r balloon)))
    (values (point-2d-x pos)
            (point-2d-y pos)
            r)))

;; - state - ;;

(defstruct (balloon-state (:include game-state))
  balloon)

(defstruct (state-expand
             (:include balloon-state
                       (process (state-lambda (balloon speed)
                                  (let ((next-state (process-state-expand balloon speed)))
                                    (setf speed
                                          (max (get-balloon-param :expand :min)
                                               (- speed (get-balloon-param :expand :brake))))
                                    next-state)))))
  (speed (get-balloon-param :expand :first)))

(defun process-state-expand (balloon speed)
  (let* ((can-expand-p (expand-balloon balloon speed))
         (id (get-entity-param balloon :client-id))
         (up-p (and (mouse-up-p id :left) (touch-summary-up-p id))))
    (when (or (not can-expand-p) up-p)
      (make-state-guard :balloon balloon))))

(defstruct (state-guard
             (:include balloon-state
                       (start-process (state-lambda (guard-time rest-guard-time)
                                        (setf rest-guard-time guard-time)))
                       (process (state-lambda (balloon guard-time rest-guard-time)
                                  (decf rest-guard-time)
                                  (update-model-in-guard-state balloon
                                                               rest-guard-time
                                                               guard-time)
                                  (when (<= rest-guard-time 0)
                                    (make-state-fragile :balloon balloon))))))
  (guard-time (get-balloon-param :guard-time))
  rest-guard-time)

(defstruct (state-fragile
             (:include balloon-state
                       (start-process (state-lambda (balloon)
                                        (add-entity-tag balloon :balloon-fragile)
                                        t))
                       (process (state-lambda (balloon)
                                  (unless (has-entity-tag balloon :balloon-fragile)
                                    (expand-balloon balloon 0)
                                    (make-state-guard :balloon balloon)))))))
