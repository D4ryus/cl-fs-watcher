;;;; package.lisp

(defpackage #:cl-fs-watcher
  (:use #:cl)
  (:export #:watcher
           ;; accessors
           #:dir
           #:hook
           #:directory-handles
           #:alive-p
           #:recursive-p

           ;; functions
           #:set-hook
           #:start-watcher
           #:stop-watcher
           #:get-all-tracked-files
           #:busy-p
           #:error-cb))
