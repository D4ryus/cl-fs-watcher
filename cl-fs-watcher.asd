;;;; cl-fs-watcher.asd

(asdf:defsystem #:cl-fs-watcher
  :description "Filesystem Watcher using cl-async."
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "LLGPL"
  :depends-on (#:cl-async)
  :serial t
  :components ((:file "package")
               (:file "cl-fs-watcher")))

