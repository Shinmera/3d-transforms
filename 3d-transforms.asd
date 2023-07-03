(asdf:defsystem 3d-transforms
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
