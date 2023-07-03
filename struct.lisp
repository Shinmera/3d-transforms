(in-package #:org.shirakumo.flare.transform)

(defstruct (transform
            (:constructor transform (&optional (location (vec 0 0 0))
                                               (scaling (vec 1 1 1))
                                               (rotation (quat 0 0 0 1))))
            (:conc-name t)
            (:copier NIL))
  (location NIL :type vec3)
  (scaling NIL :type vec3)
  (rotation NIL :type quat))

(defmethod print-object ((transform transform) stream)
  (format stream "(~s ~s ~s ~s)"
          (type-of transform)
          (tlocation transform)
          (tscaling transform)
          (trotation transform)))

(defmethod make-load-form ((transform transform) &optional env)
  (declare (ignore env))
  `(transform ,(tlocation transform) ,(tscaling transform) ,(trotation transform)))

(defun copy-transform (a)
  (transform (vcopy (tlocation a))
             (vcopy (tscaling a))
             (qcopy (trotation a))))
