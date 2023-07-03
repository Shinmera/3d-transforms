(defpackage #:org.shirakumo.flare.transform
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.flare.matrix #:org.shirakumo.flare.quaternion)
  (:import-from #:org.shirakumo.flare.vector
                #:define-ofun #:ensure-float #:*float-type*)
  (:import-from #:org.shirakumo.flare.matrix #:~=)
  ;; ops.lisp
  (:export
   #:t+
   #:nt+
   #:t=
   #:t~=
   #:t<-
   #:t*v
   #:t*p
   #:tinv
   #:t*p-inv
   #:tmix
   #:tmat4
   #:tfrom-mat
   #:tdquat
   #:tfrom-dquat
   #:tmove
   #:tmove-by
   #:toffset
   #:toffset-by
   #:trotate
   #:trotate-by
   #:tscale
   #:tscale-by
   #:tx
   #:ty
   #:tz)
  ;; struct.lisp
  (:export
   #:transform
   #:transform-p
   #:copy-transform
   #:tlocation
   #:tscaling
   #:trotation))
