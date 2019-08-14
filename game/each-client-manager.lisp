(defpackage cl-csr-jintori/game/each-client-manager
  (:use :cl
        :cl-ps-ecs
        :cl-csr-2d-game)
  (:export :update-client-manager)
  (:import-from :cl-csr-jintori/game/balloon
                :add-balloon
                :find-collided-balloon
                :try-changing-balloon-owner)
  (:import-from :cl-csr-jintori/game/parameter
                :get-param)
  (:import-from :cl-csr-jintori/game/touch-marker
                :add-touch-marker)
  (:import-from :proto-cl-client-side-rendering
                :get-new-client-id-list
                :get-deleted-client-id-list
                :mouse-down-now-p
                :get-mouse-pos
                :get-touch-summary-pos
                :touch-summary-down-now-p))
(in-package :cl-csr-jintori/game/each-client-manager)

;; --- interface --- ;;

(defun update-client-manager ()
  (dolist (id (get-new-client-id-list))
    (add-each-client-manager id))
  (dolist (id (get-deleted-client-id-list))
    (delete-each-client-manager id)))

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
      (multiple-value-bind (x y) (get-mouse-or-touch-pos id)
        (add-or-change-balloon id x y color)
        (add-touch-marker :x x :y y :color color)))))

(defun add-or-change-balloon (id x y color)
  (let* ((r (get-param :client :search-r))
         (balloon (find-collided-balloon :x x :y y :r r)))
    (if balloon
        (try-changing-balloon-owner :balloon balloon
                                    :client-id id
                                    :color color)
        (add-balloon :client-id id :x x :y y :color color))))

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

(defun get-mouse-or-touch-pos (client-id)
  ;; Note: Assume that it is called when some input is in donw-now state.
  (cond ((mouse-down-now-p client-id :left)
         (get-mouse-pos client-id))
        ((touch-summary-down-now-p client-id)
         (get-touch-summary-pos client-id))
        (t (error "No input is in donw-now state"))))
