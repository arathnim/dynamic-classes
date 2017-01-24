(defclass square (shape drawable) :class-option value
   (x 0 :type fixnum)
   (y 0 :type fixnum)
   (side-length 10)
   
   (draw ((this square))
      (with-slots (x y side-length) this
         (draw-rectangle +blue+ x y side-length side-length))))

(make-instance square :x 20 :y 50
   (draw ((this square))
      (with-slots (x y side-length) this
         (draw-rounded-rectangle +red+ x y side-length side-length))))

   macroexpansion =>

(progn
   (defclass funky-square (square)
      (draw ((this square))
         (with-slots (x y side-length) this
           (draw-rounded-rectangle +red+ x y side-length side-length))))
   (cl:make-instance 'funky-square :x 20 :y 50))
