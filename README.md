# dynamic-classes
Little library to make using CLOS significantly easier.
Expect it to be replaced by an even more advanced library at some point.

### usage

Because dynamic-classes defines it's own defclass and make-instance, you have to shadow the symbols in your own package to use them.

```lisp
(defpackage my-package
   (:shadowing-import-from dynamic-classes defclass make-instance)
	(:use cl))
```

Then, just use the new syntax in normal defclass and make-instance calls.

```lisp
(defclass square (shape drawable)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (side-length 10)
   
  (draw ((this square))
    (with-slots (x y side-length) this
      (draw-rectangle +blue+ x y side-length side-length))))

;; returns an instance of a new subclass of square,
;; with a different method for draw
(make-instance square :x 20 :y 50
  (draw ((this square))
    (with-slots (x y side-length) this
      (draw-rounded-rectangle +red+ x y side-length side-length))))
```
