#|
 This file is a part of 3d-transforms
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem 3d-transforms-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-transforms system."
  :homepage "https://Shinmera.github.io/3d-transforms/"
  :bug-tracker "https://github.com/Shinmera/3d-transforms/issues"
  :source-control (:git "https://github.com/Shinmera/3d-transforms.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-transforms :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.flare.transform.test)))
