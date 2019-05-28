(defpackage cl-csr-jintori/game/parameter
  (:use :cl
        :cl-csr-2d-game)
  (:export :get-param))
(in-package :cl-csr-jintori/game/parameter)

;; --- interface --- ;;

(defmacro get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

;; --- internal --- ;;

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar field-height 600)
  (defvar field-width (/ (* field-height 4.0) 3))

  (defun calc-absolute-length (relative-length base-length)
    (* relative-length base-length 0.001))

  "#Ex1. '#lx500' represents a half length of the field width."
  "#Ex2. '#ly500' represents a half length of the field height."
  (set-dispatch-macro-character
   #\# #\l
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\x (read-char stream)
              `(calc-absolute-length ,(read stream) field-width))
         (#\y (read-char stream)
              `(calc-absolute-length ,(read stream) field-height))
         (t (error "Not recognized character after #l"))))))

(defparameter *params*
  (convert-to-layered-hash
   (:balloon (:first-r #lx20
              :expand-speed #lx1
              :guard-time 60)
    :client (:search-r #lx10)
    :stat-graph (:width #lx40 :length #lx960
                 :x #lx20 :y #ly20))))
