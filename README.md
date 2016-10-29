# cl-fs-watcher

A Simple Filesystem Watcher to monitor a given directory for changes. This is _ALPHA_ quality software and only works on Linux (for now).

## Dependencies

- [cl-async](https://github.com/orthecreedence/cl-async) (Available through [Quicklisp](https://www.quicklisp.org/beta/))
- [stmx](https://stmx.org/) (Available through [Quicklisp](https://www.quicklisp.org/beta/))

## Installation

Just clone it into ```quicklisp/local-project``` and run ```(ql:quickload :cl-fs-watcher)```

## Examples

To start Watching ```~/watch-me/``` for changes run:

```commonlisp
(defun callback (watcher path event-type)
  (format t "something happend on watcher: ~a, which watches: ~a!~%"
            watcher (cl-fs-watcher:dir watcher))
  (format t "it happened to: ~a, event: ~a~%"
            path event-type))

(defparameter *my-watcher*
              (make-instance 'cl-fs-watcher:watcher
                             :dir "~/watch-me/" ;; watch ~/watch-me/
                             :recursive-p t     ;; also watch all subdirectories
                             :hook #'callback))

```

```callback``` will be called if something changes on ```~/watch-me/``` or its subdirectories. The First argument to ```callback``` will be the watcher object itself (here ```*my-watcher*```), path will be the absolute path to the modified file and event-type will be one of the following:
- ```:file-added``` (will always be followed by a ```:file-changed``` event with the same path)
- ```:file-removed```
- ```:file-changed```
- ```:directory-added```
- ```:directory-removed```
- ```:on-deleted``` (given if the main directory, which is watched by ```WATCHER``` (```:dir``` initarg to make-instance (here ```~/watch-me```)), is deleted. If the ```Hook``` returns (here function ```callback```) the Event Loop will finish and the Watcher ```THREAD``` will terminate)

Use ```(cl-fs-watcher:stop-watcher *my-watcher*)``` to cleanup all handles and terminate the Event-Loop. This wont be necessary if ```:on-deleted``` occurs, because the watcher will terminate himself.