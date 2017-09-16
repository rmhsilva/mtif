# MTIF

This is a Common Lisp interface to the Apple multitouch trackpad.

Usage:
- `(mtif:start #'callback-fn)` to start getting touch data.
- `(mtif:stop)` to stop getting data!

Calling `#'start` spawns a thread which repeatedly calls `#'callback-fn`,
passing the latest "frame" of multitouch data. No processing is done -- you'll
just get a list of finger data at each frame. If you want gesture recognition,
watch [this repo](https://github.com/rmhsilva/cl-gestures).

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
  size               -- A measure of the area covered
  pos-x              -- Normalised X position, 
  pos-y              -- Normalised Y position
  vel-x              -- Normalised X velocity
  vel-y              -- Normalised Y velocity
  ellipse-angle      -- Angle of the finger ellipsoid
  ellipse-major-axis -- Major axis of the finger ellipsoid
  ellipse-minor-axis -- Minor axis of the finger ellipsoid
```

## Coordinate System

Positions (`pos-x` and `pos-y`) have the origin in the bottom left corner:

```
    (0,1)              (1,1)
        +--------------+
        |              |
        |   trackpad   |
        |              |
        +--------------+
    (0,0)              (1,0)
```

The ellipse angle takes a value in the interval `[0, pi]` radians, where 0
radians means the finger is angled to right right hand edge of the trackpad:

```
              pi/2
               ^
               |
               |
      pi ------|------> 0 rad
               |
               |
              pi/2
```



## Background Info

This uses the "private" (undocumented) MacOS MultiTouch framework. See
https://gist.github.com/rmhsilva/61cc45587ed34707da34818a76476e11 for more info.

## License

MIT
