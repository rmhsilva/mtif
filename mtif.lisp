;;;; MTIF
;;;; ric@rmhsilva.com
;;;;
;;;; User API and FFI driver

(in-package :mtif)

(defvar *mtif-started* nil
  "Whether we have started using a device or not")

(defvar *mtif-handle* nil
  "Handle to the MT device, used for stopping/starting")

(defvar *mtif-callback* nil
  "User callback to call with the touchpad data")

(defvar *init-complete* nil
  "Signal whether things have been setup")

(defvar *frame-count* 0
  "The number of frames received")

(defvar *frame-limit* 0
  "Maximum number of frames to process (ignored if nil)")


(define-condition err-not-started (simple-error)
  ())

(define-condition err-already-started (simple-error)
  ())


(defstruct finger
  "Finger data"
  (id "Numeric identifier for this finger")
  (state "The finger state")
  (quality "A measure of touch quality, or pressure")
  (pos-x "Normalised X position")
  (pos-y "Normalised Y position")
  (vel-x "Normalised Y velocity")
  (vel-y "Normalised Y velocity")
  (ellipse-angle "Angle of the finger ellipsoid")
  (ellipse-major-axis "Major axis of the finger ellipsoid")
  (ellipse-minor-axis "Minor axis of the finger ellipsoid"))


(defun get-readout (mtReadout slot1 slot2)
  (getf (getf mtReadout slot1) slot2))

(defmethod translate-from-foreign (ptr (type c-finger))
  "`ptr' is to c-finger type"
  (with-foreign-slots
      ((path-id state quality normalized angle major-axis minor-axis)
       ptr (:struct Finger))
    (make-finger
     :id path-id
     :state state
     :quality quality
     :ellipse-angle angle
     :ellipse-major-axis major-axis
     :ellipse-minor-axis minor-axis
     :pos-x (get-readout normalized 'position 'x)
     :pos-y (get-readout normalized 'position 'y)
     :vel-x (get-readout normalized 'velocity 'y)
     :vel-y (get-readout normalized 'velocity 'y))))


;; call (reset) before you redefine this!
(defcallback mtcallback :int
    ((device :int)
     (data (:pointer (:struct Finger)))
     (nFingers :int)
     (timestamp :double)
     (frame :int))
  (declare (ignore device))
  (when (or (not (numberp *frame-limit*))
             (< *frame-count* *frame-limit*))
    (incf *frame-count*)
    (unless (null *mtif-callback*)
      (let ((finger-data (loop for i from 0 to (1- nFingers)
                               collect (mem-aref data '(:struct Finger) i))))
        (funcall (symbol-value '*mtif-callback*) finger-data timestamp frame))))
  #|return |# 0)


(defun print-all-touches (finger-data timestamp frame)
  "Example callback which just prints all touches to *standard-output*"
  (format t "~A [~{~A~%  ~}]~%" (length finger-data) finger-data))


(defun init-device ()
  "Initial setup - create device, register callback"
  (setf *mtif-handle* (mtdevicecreatedefault))
  (mtregistercontactframecallback *mtif-handle*
                                  (get-callback 'mtcallback))
  (setf *init-complete* t))


(defun start (callback &optional frame-limit)
  "Start the MT interface with `callback' receiving the data"
  (unless *init-complete* (init-device))
  (if *mtif-started* (error 'err-already-started))
  (setf *mtif-callback* callback)
  (setf *frame-count* 0)
  (setf *frame-limit* frame-limit)
  (mtdevicestart *mtif-handle* 0)
  (setf *mtif-started* t))

(defun stop ()
  "Stop the MT interface"
  (unless *mtif-started* (error 'err-not-started))
  (setf *mtif-started* nil)
  (mtdevicestop *mtif-handle*))


(defun reset ()
  "Reset the device"
  (if *init-complete*
      (progn (handler-case
                 (stop)
               (err-not-started ()))
             (mtunregistercontactframecallback *mtif-handle*
                                               (get-callback 'mtcallback))
             (setf *init-complete* nil))
      (warn "Not initialised")))
