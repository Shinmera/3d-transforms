#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.transform)

(defun t+ (a b)
  (transform
   (nv+ (q*v (transform-rotation a) (v* (transform-scale a) (transform-location b)))
        (transform-location a))
   (v* (transform-scale a) (transform-scale b))
   (q* (transform-rotation a) (transform-rotation b))))

(defun nt+ (a b)
  (nv+ (transform-location a)
       (q*v (transform-rotation a) (v* (transform-scale a) (transform-location b))))
  (nv* (transform-scale a) (transform-scale b))
  (nq* (transform-rotation a) (transform-rotation b))
  a)

(defun t*v (a v)
  (q*v (transform-rotation a) (v* (transform-scale a) v)))

(defun t*p (a v)
  (nv+ (t*v a v) (transform-location a)))

(defun tinv (a)
  (flet ((save-invert (x)
           (if (< (abs x) single-float-epsilon) 0.0 (/ x))))
    (let ((invrot (qinv (transform-rotation a)))
          (scale (transform-scale a))
          (invscale (vec (save-invert (vx scale))
                         (save-invert (vy scale))
                         (save-invert (vz scale)))))
      (transform
       (q*v invrot (v* invscale (transform-location a) -1))
       invscale
       invrot))))

(defun tmix (from to x)
  (let ((rot (transform-rotation to)))
    (when (< (q. (transform-rotation from) rot) 0.0)
      (setf rot (q- rot)))
    (transform
     (vlerp (transform-location from) (transform-location to) x)
     (vlerp (transform-scale from) (transform-scale to) x)
     (qnlerp (transform-rotation from) rot x))))

(defun tmat4 (a)
  (let* ((l (transform-location a))
         (s (transform-scale a))
         (r (transform-rotation a))
         (x (nv* (q*v r +vx+) (vx s)))
         (y (nv* (q*v r +vy+) (vy s)))
         (z (nv* (q*v r +vz+) (vz s))))
    (mat4 (vx x) (vx y) (vx z) (vx l)
          (vy x) (vy y) (vy z) (vy l)
          (vz x) (vz y) (vz z) (vz l)
          0 0 0 1)))

(defun tfrom-mat (mat)
  (with-fast-matref (m mat 4)
    (let* ((rot (qfrom-mat mat))
           (mat3 (nm* (mat3 (m 0 0) (m 1 0) (m 2 0)
                            (m 0 1) (m 1 1) (m 2 1)
                            (m 0 2) (m 1 2) (m 2 2))
                      (qmat3 (qinv rot)))))
      (with-fast-matref (n mat3 3)
        (transform
         (vec (m 4 0) (m 4 1) (m 4 2))
         (vec (n 0 0) (n 1 1) (n 2 2))
         rot)))))

(defun tmove (a v)
  (nv+ (transform-location a) v)
  a)

(defun tmove-by (a x y z)
  (incf (vx (transform-location a)) x)
  (incf (vy (transform-location a)) y)
  (incf (vz (transform-location a)) z)
  a)

(defun tscale (a v)
  (nv+ (transform-scale a) v)
  a)

(defun tscale-by (a x y z)
  (incf (vx (transform-scale a)) x)
  (incf (vy (transform-scale a)) y)
  (incf (vz (transform-scale a)) z)
  a)

(defun trotate (a q)
  (nq* (transform-rotation a) q)
  a)

(defun trotate-by (a axis angle)
  (nq* (transform-rotation a) (qfrom-angle axis angle))
  a)
