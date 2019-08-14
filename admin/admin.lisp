(defpackage cl-csr-jintori/admin/admin
  (:use :cl
        :cl-markup)
  (:import-from :cl-csr-jintori/server
                :start-game)
  (:import-from :cl-csr-2d-game
                :get-ningle-app)
  (:import-from :lack.response
                :response-headers
                :response-status)
  (:import-from :ningle
                :route
                :*response*)
  (:import-from :quri
                :url-encode))
(in-package :cl-csr-jintori/admin/admin)

(defun get-param (param-name params)
  (cdr (assoc param-name params :test #'string=)))

(defun redirect (url)
  (setf (getf (response-headers *response*) :location) url)
  (setf (response-status *response*) 302)
  url)

(defun redirect-to-error (message)
  (redirect (format nil "/admin/error/~D" (url-encode message))))

(progn
  (defun make-error-route ()
    (lambda (params)
      (setf (response-status *response*) 400)
      (with-output-to-string (str)
        (let ((cl-markup:*output-stream* str))
          (html5 (:head
                  (:title "Error"))
                 (:body
                  (:div (format nil "ERROR!!: ~A"
                                (cdr (assoc :message params))))))))))

  (setf (route (get-ningle-app) "/admin/error/:message")
        (make-error-route)))

(progn
  (defun make-index-route ()
    (lambda (params)
      (declare (ignore params))
      (with-output-to-string (str)
        (let ((cl-markup:*output-stream* str))
          (html5 (:head
                  (:title "Admin console for cl-csr-jintori"))
                 (:body
                  (:h2 "Client List")
                  (:div "WIP")
                  (:h2 "Restart Game")
                  (:form
                   :action "/admin/restart" :method "POST"
                   (:input :type "submit" :value "Restart"))))))))

  (setf (route (get-ningle-app) "/admin")
        (make-index-route)))

(progn
  (defun make-restart-route ()
    (lambda (params)
      (declare (ignore params))
      (start-game)
      (redirect "/admin")))

  (setf (route (get-ningle-app) "/admin/restart" :method :post)
        (make-restart-route)))
