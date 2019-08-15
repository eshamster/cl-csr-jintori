(defpackage cl-csr-jintori/main
  (:nicknames :cl-csr-jintori)
  (:use :cl
        :cl-csr-jintori/server
        :cl-csr-jintori/admin/admin)
  (:export :start-game
           :stop-game))
