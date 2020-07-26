#|
  This file is a part of cl-csr-jintori project.
  Copyright (c) 2019 eshamster (hamgoostar@gmail.com)
|#

#|
  Jintori game using client side rendering (CSR)

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem "cl-csr-jintori"
  :version "0.1.0"
  :author "eshamster"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :license "MIT"
  :depends-on (:cl-markup
               :ningle
               :cl-csr
               :cl-csr-2d-game
               :cl-csr-jintori/main)
  :description "Jintori game using client side rendering (CSR)")
