;;;; MTIF
;;;; ric@rmhsilva.com
;;;;
;;;; CFFI definitions

(in-package :mtif)

(define-foreign-library multitouch-support
  (:darwin "/System/Library/PrivateFrameworks/MultitouchSupport.framework/MultitouchSupport"))

(use-foreign-library multitouch-support)


(defctype MTDeviceRef :pointer)
(defctype MTContactCallbackFunction :pointer)

(defcstruct mtPoint
  "Point coordinates on the trackpad"
  (x :float)
  (y :float))

(defcstruct mtReadout
  "Other trackpad readout?"
  (position (:struct mtPoint))
  (velocity (:struct mtPoint)))

(defcenum mtState
  "Finger state"
  :not-tracking
  :start-in-range
  :hover-in-range
  :make-touch
  :touching
  :break-touch
  :linger-in-range
  :out-of-range)

(defcstruct (Finger :class c-finger)
  "One of these for every finger on the trackpad"
  (frame :int)
  (timestamp :double)
  (path-id :int)
  (state mtState)
  (finger-id :int)
  (hand-id :int)                        ; always 1?
  (normalized (:struct mtReadout))
  (size :float)
  (zero1 :int)
  (angle :float)
  (major-axis :float)
  (minor-axis :float)
  (absolute (:struct mtReadout))
  (zero2 :int)
  (zero3 :int)
  (unknown2 :float))


(defcfun "MTDeviceCreateDefault" MTDeviceRef)

(defcfun "MTRegisterContactFrameCallback" :void
  (device MTDeviceRef)
  (callback MTContactCallbackFunction))

(defcfun "MTUnregisterContactFrameCallback" :void
  (device MTDeviceRef)
  (callback MTContactCallbackFunction))

(defcfun "MTDeviceStart" :void
  (device MTDeviceRef)
  (unused :int))

(defcfun "MTDeviceStop" :void
  (device MTDeviceRef))
