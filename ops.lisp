#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.transform)

(declaim (type (function (transform transform) transform) t+ nt+))
(define-ofun t+ (a b)
  (transform
   (nv+ (q*v (trotation a) (v* (tscaling a) (tlocation b)))
        (tlocation a))
   (v* (tscaling a) (tscaling b))
   (q* (trotation b) (trotation a))))

(define-ofun nt+ (a b)
  (nv+ (tlocation a)
       (q*v (trotation a) (v* (tscaling a) (tlocation b))))
  (nv* (tscaling a) (tscaling b))
  (q<- (trotation a) (q* (trotation b) (trotation a)))
  a)

(declaim (type (function (transform transform) boolean) t~=))
(define-ofun t= (a b)
  (and (v= (tlocation a) (tlocation b))
       (v= (tscaling a) (tscaling b))
       (q= (trotation a) (trotation b))))

(define-ofun t~= (a b)
  (flet ((v~= (a b)
           (and (~= (vx a) (vx b))
                (~= (vy a) (vy b))
                (~= (vz a) (vz b)))))
    (and (v~= (tlocation a) (tlocation b))
         (v~= (tscaling a) (tscaling b))
         (qequal (trotation a) (trotation b)))))

(define-ofun t<- (target source)
  (v<- (tlocation target) (tlocation source))
  (v<- (tscaling target) (tscaling source))
  (q<- (trotation target) (trotation source)))

(declaim (type (function (transform vec3) vec3) t*v t*p))
(declaim (inline t*v t*p))
(define-ofun t*v (a v)
  (q*v (trotation a) (v* (tscaling a) v)))

(define-ofun t*p (a v)
  (nv+ (q*v (trotation a) (v* (tscaling a) v)) (tlocation a)))

(declaim (type (function (transform) transform) tinv))
(define-ofun tinv (a)
  (flet ((save-invert (x)
           (if (< (abs x) single-float-epsilon) 0.0 (/ x))))
    (let* ((invrot (qinv (trotation a)))
           (scale (tscaling a))
           (invscale (vec (save-invert (vx scale))
                          (save-invert (vy scale))
                          (save-invert (vz scale)))))
      (transform
       (q*v invrot (v* invscale (tlocation a) -1))
       invscale
       invrot))))

(declaim (type (function (transform transform real) transform) tmix))
(define-ofun tmix (from to x)
  (let ((rot (trotation to))
        (x (ensure-float x)))
    (when (< (q. (trotation from) rot) 0.0)
      (setf rot (q- rot)))
    (transform
     (vlerp (tlocation from) (tlocation to) x)
     (vlerp (tscaling from) (tscaling to) x)
     (qnlerp (trotation from) rot x))))

(declaim (type (function (transform &optional mat4) mat4) tmat4))
(define-ofun tmat4 (a &optional (mat (mat4)))
  (let* ((l (tlocation a))
         (s (tscaling a))
         (r (trotation a))
         (x (qx r)) (y (qy r)) (z (qz r)) (w (qw r))
         (xx (* x x)) (xy (* x y)) (xz (* x z)) (xw (* x w))
         (yy (* y y)) (yz (* y z)) (yw (* y w))
         (zz (* z z)) (zw (* z w)))
    (msetf mat
           (* (vx3 s) (- 1 (* 2 (+ yy zz)))) (* (vy3 s) 2 (- xy zw)) (* (vz3 s) 2 (+ xz yw)) (vx3 l)
           (* (vx3 s) 2 (+ xy zw)) (* (vy3 s) (- 1 (* 2 (+ xx zz)))) (* (vz3 s) 2 (- yz xw)) (vy3 l)
           (* (vx3 s) 2 (- xz yw)) (* (vy3 s) 2 (+ yz xw)) (* (vz3 s) (- 1 (* 2 (+ xx yy)))) (vz3 l)
           0.0 0.0 0.0 1.0)))

(declaim (type (function (mat4) transform) tfrom-mat))
(define-ofun tfrom-mat (mat)
  (with-fast-matref (m mat 4)
    (let* ((rot (qfrom-mat mat))
           (mat3 (nm* (mat (m 0 0) (m 0 1) (m 0 2)
                           (m 1 0) (m 1 1) (m 1 2)
                           (m 2 0) (m 2 1) (m 2 2))
                      (qmat3 (qinv rot)))))
      (with-fast-matref (n mat3 3)
        (transform
         (vec (m 0 3) (m 1 3) (m 2 3))
         (vec (n 0 0) (n 1 1) (n 2 2))
         rot)))))

(declaim (type (function (transform vec3) transform) tmove toffset tscale))
(declaim (inline tmove tmove-by toffset toffset-by tscale tscale-by trotate trotate-by))
(define-ofun tmove (a v)
  (nv+ (tlocation a) (q*v (trotation a) (v* (tscaling a) v)))
  a)

(declaim (type (function (transform real real real) transform) tmove-by toffset-by tscale-by))
(define-ofun tmove-by (a x y z)
  (nv+ (tlocation a) (q*v (trotation a) (nv* (vec x y z) (tscaling a))))
  a)

(define-ofun toffset (a v)
  (nv+ (tlocation a) v)
  a)

(define-ofun toffset-by (a x y z)
  (incf (vx3 (tlocation a)) (ensure-float x))
  (incf (vy3 (tlocation a)) (ensure-float y))
  (incf (vz3 (tlocation a)) (ensure-float z))
  a)

(define-ofun tscale (a v)
  (nv+ (tscaling a) v)
  a)

(define-ofun tscale-by (a x y z)
  (incf (vx3 (tscaling a)) (ensure-float x))
  (incf (vy3 (tscaling a)) (ensure-float y))
  (incf (vz3 (tscaling a)) (ensure-float z))
  a)

(declaim (type (function (transform quat) transform) trotate))
(define-ofun trotate (a q)
  (nq* (trotation a) q)
  a)

(declaim (type (function (transform vec3 real) transform) trotate-by))
(define-ofun trotate-by (a axis angle)
  (nq* (trotation a) (qfrom-angle axis angle))
  a)
