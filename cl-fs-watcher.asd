;;;; cl-fs-watcher.asd

(asdf:defsystem #:cl-fs-watcher
  :description "Filesystem Watcher using cl-async."
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "LLGPL"
  :homepage "https://github.com/d4ryus/cl-fs-watcher"
  :depends-on (#:cl-async #:lparallel)
  :serial t
  :components ((:file "package")
               (:file "cl-fs-watcher")))

