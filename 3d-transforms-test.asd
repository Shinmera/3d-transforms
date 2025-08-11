(asdf:defsystem 3d-transforms-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-transforms system."
  :homepage "https://shinmera.com/docs/3d-transforms/"
  :bug-tracker "https://shinmera.com/project/3d-transforms/issues"
  :source-control (:git "https://shinmera.com/project/3d-transforms.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-transforms :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.flare.transform.test)))
