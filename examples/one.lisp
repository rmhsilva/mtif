;;;; MTIF
;;;; ric@rmhsilva.com
;;;;
;;;; Example - display all touches


(defpackage :mtif-example-one
  (:use :mtif :cl :sketch)
  (:export :run))

(in-package :mtif-example-one)


(defvar *fingers* nil)


(defun print-finger-pos (f &optional (x 20) (y 20))
  "Print finger data at offset"
  (let* ((size 14)
         (y (- y size 2)))
    (with-font (make-font :color (gray 0.4) :size size)
      (text (format nil "@~d" (finger-id f))
            x y)
      (text (format nil "~2$ (~2$, ~2$)"
                    (finger-ellipse-angle f) (finger-pos-x f) (finger-pos-y f))
            x (+ y size)))))

(defun clamp (x max min)
  (cond ((> x max) max)
        ((< x min) min)
        (t x)))

(defun norm-quality (f)
  (/ (clamp (finger-size f) 2.0 0.5)
     1.5))

(defsketch fingers
    ((title "Fingers")
     (width 400)
     (height 400))
  (background (gray 0.9))
  (dolist (f *fingers*)
    (let ((angle (finger-ellipse-angle f))
          (cx (* width (finger-pos-x f)))
          ;; coordinate system is different in sketch
          (cy (* height (- 1.0 (finger-pos-y f))))
          (rx (* 2.0 (finger-ellipse-minor-axis f)))
          (ry (* 2.0 (finger-ellipse-major-axis f))))
      (with-pen (make-pen :fill (rgb-255 #x2e #x8b #x57 (+ 155.0 (* 100.0 (norm-quality f)))))
        ;; Rotate from identity to prevent fingers affecting each other
        (with-identity-matrix
          (print-finger-pos f (+ cx (max rx ry) 2) cy)
          (rotate (- 90.0 (degrees angle)) cx cy)
          (ellipse cx cy rx ry))))))



(defun callback (finger-data timestamp frame)
  (setf *fingers* finger-data))

(defun run ()
  (make-instance 'fingers)
  (mtif:start #'callback))
