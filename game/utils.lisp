(defpackage cl-csr-jintori/game/utils
  (:use :cl)
  (:export :out-of-screen-p)
  (:import-from :cl-csr-jintori/game/parameter
                :get-field-width
                :get-field-height))
(in-package :cl-csr-jintori/game/utils)

;; --- interface --- ;;

(defun out-of-screen-p (x y r)
  (or (> (+ x r) (get-field-width))
      (< (- x r) 0)
      (> (+ y r) (get-field-height))
      (< (- y r) 0)))

;; --- internal --- ;;

