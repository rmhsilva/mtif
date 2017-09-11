# MTIF

This is a Common Lisp interface to the Apple multitouch trackpad.

Usage:
- `(start callback-fn)` to start getting touch data.
- `(stop)` to stop getting data!


## Data Format

An example callback function:

```lisp
(defun print-all-touches (finger-data timestamp frame)
  "Example callback which just prints all touches to *standard-output*"
  (format t "~A [~{~A~%  ~}]~%" (length finger-data) finger-data))
```

The finger data struct has the following slots:

```
  id                 -- Numeric identifier for this finger
  state              -- The finger state
  quality            -- A measure of touch quality, or pressure
  pos-x              -- Normalised X position
  pos-y              -- Normalised Y position
  vel-x              -- Normalised Y velocity
  vel-y              -- Normalised Y velocity
  ellipse-angle      -- Angle of the finger ellipsoid
  ellipse-major-axis -- Major axis of the finger ellipsoid
  ellipse-minor-axis -- Minor axis of the finger ellipsoid
```


## License

MIT
