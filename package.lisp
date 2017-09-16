;;;; MTIF
;;;; ric@rmhsilva.com

(defpackage :mtif
  (:use :cl :cffi)
  (:export
   :start
   :stop
   :*frame-count*
   :*mtif-started*

   ;; example/test callback
   :print-all-touches

   ;; finger struct slots
   :finger-id
   :finger-state
   :finger-size
   :finger-pos-x
   :finger-pos-y
   :finger-vel-x
   :finger-vel-y
   :finger-ellipse-angle
   :finger-ellipse-major-axis
   :finger-ellipse-minor-axis

   :err-not-started
   :err-already-started
   ))

(defpackage :mtif-user
  (:use :cl :mtif))
