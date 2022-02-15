#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.transform.test
  (:use #:cl #:parachute
        #:org.shirakumo.flare.vector
        #:org.shirakumo.flare.matrix
        #:org.shirakumo.flare.quaternion
        #:org.shirakumo.flare.transform)
  (:import-from #:org.shirakumo.flare.matrix #:~=))
(in-package #:org.shirakumo.flare.transform.test)

(define-test 3d-transforms)

(define-test struct
  :parent 3d-transforms
  (of-type transform (transform))
  (of-type transform (transform (vec 1 2 3) (vec 4 5 6) (quat 7 8 9 0)))
  (true (transform-p (transform)))
  (is v= (vec 0 0 0) (tlocation (transform)))
  (is v= (vec 1 1 1) (tscaling (transform)))
  (is q= (quat 0 0 0 1) (trotation (transform))))

(define-test arithmetic
  :parent 3d-transforms
  :depends-on (struct)
  (is t~= (transform) (t+ (transform (vec 1 2 3)) (transform (vec -1 -2 -3))))
  )

(define-test translation
  :parent 3d-transforms
  :depends-on (struct)
  (is m~= (meye 4) (tmat4 (transform)))
  (is m~= (mtranslation (vec 1 2 3)) (tmat4 (transform (vec 1 2 3))))
  (is m~= (mscaling (vec 1 2 3)) (tmat4 (transform (vec 0 0 0) (vec 1 2 3))))
  (is m~= (mrotation +vx+ PI) (tmat4 (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI))))
  (is t~= (transform) (tfrom-mat (meye 4)))
  (is t~= (transform (vec 1 2 3)) (tfrom-mat (mtranslation (vec 1 2 3))))
  (is t~= (transform (vec 0 0 0) (vec 1 2 3)) (tfrom-mat (mscaling (vec 1 2 3))))
  (is t~= (transform (vec 0 0 0) (vec 1 1 1) (qfrom-angle +vx+ PI)) (tfrom-mat (mrotation +vx+ PI))))
