;;;; MTIF
;;;; ric@rmhsilva.com

(asdf:defsystem #:mtif
  :version      "0.1.0"
  :description  "An interface to the MacOS MultiTouch framework"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :components ((:file "package")
               (:file "mtif-ffi")
               (:file "mtif"))

  :depends-on (#:cffi))
