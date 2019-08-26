(defpackage cl-csr-jintori/game/parameter
  (:use :cl
        :cl-csr-2d-game)
  (:export :get-param
           :get-depth
           :get-field-width
           :get-field-height))
(in-package :cl-csr-jintori/game/parameter)

;; --- interface --- ;;

(defmacro get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

(defmacro get-depth (&rest keys)
  `(get-layered-hash *depth* ,@keys))

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
              :guard-time 60
              :edge-color #xff0000)
    :client (:color-table '(#x00bbbb #xbb00bb #xbbbb00))
    ;; Assume that valid client id is positive number
    :nil-owner (:id -1 :color #x888888)
    :stat-graph (:width #lx40 :length #lx960
                 :x #lx20 :y #ly20)
    :marker (:global (:duration 10 :r #lx10)
             :local (:duration 30 :length #ly400 :color #xff0000)))))

(defparameter *depth*
  (convert-to-layered-hash
   (:balloon 10
    :balloon-edge 15
    :marker 100
    :stat (:background 20 :foreground 30))))

(defun get-field-width ()
  #lx1000)

(defun get-field-height ()
  #ly1000)
