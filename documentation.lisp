#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.transform)

;; struct.lisp
(docs:define-docs
  (type transform
    "Encapsulation of a spatial transform.

A transform holds a translation, rotation, and scaling.
By nesting transforms you can create relative coordinate systems.

A transform is readable and dumpable, meaning it can be inlined at
compile-time and printed and read back.

See TRANSFORM (function)
See TRANSFORM-P
See COPY-TRANSFORM
See TLOCATION
See TSCALING
See TROTATION
See T+
See NT+
See T*V
See T*P
See TINV
See TMIX
See TMAT4
See TFROM-MAT
See TMOVE
See TMOVE-BY
See TOFFSET
See TOFFSET-BY
See TROTATE
See TROTATE-BY
See TSCALE
See TSCALE-BY")
  
  (function transform
    "Creates a new transform.

Note that the given location, scale, and rotation are shared and NOT
copied. Any change to them will thus reflect within the transform.

If not passed, they default to \"neutral\" values, meaning:
  0,0,0 for location,
  1,1,1 for scale,
  0,0,0,1 for rotation

See TRANSFORM (type)")
  
  (function transform-p
    "Returns true if the given value is a TRANSFORM structure.

See TRANSFORM (type)")
  
  (function copy-transform
    "Creates a deep copy of the transform.

The returned transform does NOT share its location, scale, or rotation
components.

See TRANSFORM (type)")
  
  (function tlocation
    "Accesses the location or position component of the transform.

See TRANSFORM (type)")
  
  (function tscaling
    "Accesses the scaling component of the transform.

See TRANSFORM (type)")
  
  (function trotation
    "Accesses the rotational component of the transform.

See TRANSFORM (type)"))

;; ops.lisp
(docs:define-docs
  (function t+
    "Returns a transform that encompasses the combination of transform B on top of A.

This is particularly important in regard to the location expressed by
the transform, as the rotation has to be taken into account properly.

See NT+
See TRANSFORM (type)")

  (function nt+
    "Modifies the first transform such that it encompasses the transform B on top of itself.


See T+
See TRANSFORM (type)")
  
  (function t*v
    "Applies the transform to the given vector.

This scales and rotates the vector, but does not translate it.

See T*P
See VEC3 (type)
See TRANSFORM (type)")
  
  (function t*p
    "Applies the transform to the given point.

This transforms the point completely by the coordinate system
represented by the transform.

See T*P
See VEC3 (type)
See TRANSFORM (type)")
  
  (function tinv
    "Returns the inverses of the given transform.

This is not a precise inversion, instead performing an approximation
for speed.

See TRANSFORM (type)")
  
  (function tmix
    "Returns a new transform that is the linear interpolation between the two at the given point.

X should be a REAL in the range of [0,1].

See TRANSFORM (type)")
  
  (function tmat4
    "Returns a 4x4 matrix that encompasses the same transformation as the transform's components encompass.

See MAT4 (type)
See TRANSFORM (type)")
  
  (function tfrom-mat
    "Returns a transform that represents the same linear transformation as the matrix, if possible.

See MAT4 (type)
See TRANSFORM (type)")
  
  (function tmove
    "Moves the transform space by the given vector.

This is equivalent to

  (nt+ a (transform v))

Note that as a consequence, this is relative to the rotation
encompassed by the transform.

See TMOVE-BY
See TOFFSET
See VEC3 (type)
See TRANSFORM (type)")
  
  (function tmove-by
    "Moves the transform space by the given amount in each axis.

This is equivalent to

  (nt+ a (transform (vec x y z)))

Note that as a consequence, this is relative to the rotation
encompassed by the transform.

See TMOVE
See TOFFSET-BY
See VEC3 (type)
See TRANSFORM (type)")
  
  (function toffset
    "Offsets the transform in the basic cartesian space by the given vector.

This is equivalent to

  (nv+ (tlocation a) v)

Note that as a consequence, this is NOT relative to the rotation
encompassed by the transform.

See TMOVE
See TOFFSET-BY
See VEC3 (type)
See TRANSFORM (type)")
  
  (function toffset-by
    "Offsets the transform in the basic cartesian space by the given amount in each axis.

This is equivalent to

  (nv+ (tlocation a) (vec x y z))

Note that as a consequence, this is NOT relative to the rotation
encompassed by the transform.

See TMOVE
See TOFFSET-BY
See VEC3 (type)
See TRANSFORM (type)")

  (function tscale
    "Scales the transform space by the given vector.

This is equivalent to

  (nv+ (tscaling a) v)

See TSCALE-BY
See VEC3 (type)
See TRANSFORM (type)")
  
  (function tscale-by
    "Scales the transform space by the given amount in each axis.

This is equivalent to

  (nv+ (tscaling a) (vec x y z))

See TSCALE
See VEC3 (type)
See TRANSFORM (type)")
  
  (function trotate
    "Rotates the transform space by the given quaternion.

This is equivalent to

  (nq* (trotation a) q)

See TROTATE-BY
See QUAT (type)
See TRANSFORM (type)")
  
  (function trotate-by
    "Rotates the transform space around the given axis by the given angle.

This is equivalent to

  (nq* (trotation a) (qfrom-angle axis angle))

See TROTATE
See QUAT (type)
See TRANSFORM (type)"))
