# dynamic-classes
Little library to make using CLOS significantly easier.
Expect it to be replaced by a more advanced library at some point.

### usage

Because dynamic-classes defines it's own defclass and make-instance, you have to shadow the symbols in your own package to use them.

```lisp
(defpackage my-package
  (:shadowing-import-from dynamic-classes defclass make-instance)
  (:use cl))

(in-package my-package)
```

Then, just use the new syntax in normal defclass and make-instance calls.

```lisp
(defclass square (shape drawable)
  (x 0 :type single-float)
  (y 0 :type single-float)
  (side-length 10 :type fixnum)

  (draw ((this square))
    (with-slots (x y side-length) this
      (draw-rectangle +blue+ x y (+ x side-length) (+ y side-length)))))

;; returns an instance of the square class with the given values
(make-instance square :x 20 :y 30 :side-length 100)

;; returns an instance of a new subclass of square,
;; with a new method for draw
(make-instance square :x 20 :y 50
  (draw (*this*)
    (with-slots (x y side-length) this
      (draw-rounded-rectangle +red+ x y (+ x side-length) (+ y side-length)))))
```

The classes made by defclass have automatic accessor and initarg values, so don't worry about supplying those.