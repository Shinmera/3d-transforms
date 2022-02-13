#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem 3d-transforms
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing a common structure to encapsulate spatial transformations"
  :homepage "https://Shinmera.github.io/3d-transforms/"
  :bug-tracker "https://github.com/Shinmera/3d-transforms/issues"
  :source-control (:git "https://github.com/Shinmera/3d-transforms.git")
  :serial T
  :components ((:file "package")
               (:file "struct")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors
               :3d-matrices
               :3d-quaternions)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-transforms-test))))
