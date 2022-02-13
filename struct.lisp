#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.transform)

(defstruct (transform
            (:constructor (transform (&optional (location (vec 0 0 0))
                                                (scale (vec 1 1 1))
                                                (rotation (quat 0 0 1)))))
            (:copier NIL))
  (location NIL :type vec3)
  (scale NIL :type vec3)
  (rotation NIL :type quat))

(defmethod print-object ((transform transform) stream)
  (format stream "(~s ~s ~s ~s)"
          (type-of transform)
          (location transform)
          (scale transform)
          (rotation transform)))

(defmethod make-load-form ((transform transform) &optional env)
  (declare (ignore env))
  `(transform ,(transform-location transform) ,(transform-scale transform) ,(transform-rotation transform)))

(defun copy-transform (a)
  (transform (vcopy (transform-location a))
             (vcopy (transform-scale a))
             (qcopy (transform-rotation a))))
