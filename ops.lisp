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
   (q* (trotation a) (trotation b))))

(define-ofun nt+ (a b)
  (nv+ (tlocation a)
       (q*v (trotation a) (v* (tscaling a) (tlocation b))))
  (nv* (tscaling a) (tscaling b))
  (nq* (trotation a) (trotation b))
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

(declaim (type (function (transform vec3) vec3) t*v t*p))
(declaim (inline t*v t*p))
(define-ofun t*v (a v)
  (q*v (trotation a) (v* (tscaling a) v)))

(define-ofun t*p (a v)
  (nv+ (t*v a v) (tlocation a)))

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

(declaim (type (function (transform) mat4) tmat4))
(define-ofun tmat4 (a)
  (let* ((l (tlocation a))
         (s (tscaling a))
         (r (trotation a))
         (x (nv* (q*v r +vx+) (vx s)))
         (y (nv* (q*v r +vy+) (vy s)))
         (z (nv* (q*v r +vz+) (vz s))))
    (mat (vx3 x) (vx3 y) (vx3 z) (vx3 l)
         (vy3 x) (vy3 y) (vy3 z) (vy3 l)
         (vz3 x) (vz3 y) (vz3 z) (vz3 l)
         0 0 0 1)))

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
