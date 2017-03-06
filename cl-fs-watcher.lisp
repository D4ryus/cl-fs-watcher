;;;; cl-fs-watcher.lisp

#|

This file contains the directory watcher which is used to watch a
directory for changes to its files and directories.

To use the Watcher create a instance of WATCHER with a directory to
watch and attach a hook. Once a Watcher is created a Thread is started
which is handling the event-loop (see cl-async documentation for more
information about the event-loop)

For example:

(defparameter *my-watcher*
              (make-instance 'watcher
                             :dir "~/watch-me/" ;; watch ~/watch-me/
                             :recursive-p t     ;; also watch all subdirectories of ~/watch-me/
                             :hook (lambda (watcher path event-type) ;; call this function if anything happens
                                     (format t "something happend on watcher: ~a, which watches: ~a!~%"
                                             watcher (dir watcher))
                                     (format t "it happened to: ~a, event: ~a~%"
                                             path event-type))
                             :error-cb (lambda (ev)
                                         (format t "ERROR: ~a" ev)
                                         (stop-watcher *my-watcher*))))

(start-watcher *my-watcher*)

path is the absolute path to the changed file, so if i-was-changed.txt
inside ~/watch-me/some-dir/ is changed, the path will be (if $HOME is
/home/steve):
/home/steve/watch-me/some-dir/i-was-changed.txt

type (the last argument to the hook function) will be one of the
following:
   :file-added (will always be followed by a :file-changed event with
                the same path)
   :file-removed
   :file-changed
   :directory-added
   :directory-removed
   :on-deleted (given if the main directory, which is watched by
                WATCHER (:dir initarg to make-instance), is
                deleted. If the Hook returns the Event Loop will
                finish and the Watcher THREAD will terminate)

To disable/remove the hook use:

(setf (hook *my-watcher*) nil)

and to stop the Watcher and cleanup all its resources use:

(stop-watcher *my-watcher*)

|#

(in-package #:cl-fs-watcher)

;;; "cl-fs-watcher" goes here. Hacks and glory await!

(defclass watcher ()
  ((dir :reader dir
        :initarg :dir
        :initform (error "specify a directory!")
        :type string
        :documentation "Main or Root Directory which will be watched,
        if RECURSIVE-P is t all its subdirectories will be watched
        too.")
   (skip-duplicated :reader skip-duplicated
                    :initarg :skip-duplicated
                    :initform t
                    :type boolean
                    :documentation "Flag to skip duplicated events. If
                    set to t hook will not be called when the current
                    event equals the next event. For example if the
                    current event is :file-changed \"/blub.txt\" and
                    the next event is also :file-changed
                    \"/blub.txt\", then the first event will be
                    skipped and hook will not be called.")
   (thread :reader thread
           :type bt:thread
           :initform nil
           :documentation "BT:THREAD which will run the event-loop, on
           Creation of WATCHER the THREAD will be created. Thread will
           finish if DIR gets deleted or STOP-WATCHER is called.")
   (hook-queue :reader hook-queue
               :type lparallel.queue:queue
               :initform (lparallel.queue:make-queue)
               :documentation "Queue which holds all hooks in order,
               the event-loop will push hooks onto it and HOOK-THREAD
               will consume and run them.")
   (event-loop-busy-p :type boolean
                      :initform nil
                      :documentation "Boolean (t or nil) which is set
                      to t when the event-loop is currently processing
                      a event (callback is called). If
                      event-loop-busy-p is nil, the event-loop is
                      waiting for new events.")
   (hook-busy-p :type boolean
                :initform nil
                :documentation "Boolean (t or nil) which is set to t
                when hook-thread is currently processing a hook (aka
                is busy). If hook-busy-p is nil, the Hook-Thread is
                waiting for new events.")
   (hook-thread :reader hook-thread
                :initform nil
                :type bt:thread
                :documentation "BT:THREAD which calls the attached
                hook so that the event loop does not get blocked on
                longer running hooks.")
   (hook :accessor hook
         :initarg :hook
         :initform nil
         :type function
         :documentation "The function which gets called if a event
         occurs. HOOk needs to be a FUNCTION which takes 3
         arguments. It will be called with the Watcher Object, the
         filename and the Event-type.

         Setting HOOK to NIL will disable it.

         Event-type is one of the following:

         :file-added (will always be followed by a :file-changed event
                     with the same path)
         :file-removed
         :file-changed
         :directory-added
         :directory-removed
         :on-deleted (given if the main directory, which is watched by
                     WATCHER (:dir initarg to make-instance), is
                     deleted. If the Hook returns the Event Loop will
                     finish and the Watcher THREAD will terminate)

         If a directory is added and RECURSIVE-P is true, the
         directory will automatically be added to the watched list. If
         a subdirectory gets deleted the handle will be deleted
         too. So there is no need to handle those.

         Example:
         (setf (hook my-watcher-obj)
               (lambda (watcher path event-type)
                 (format t \"Hook from Watcher ~a was called!~%\"
                         watcher)
                 (format t \"File ~a, Event: ~a!~%\"
                         path event-type)))")
   (directory-handles :reader directory-handles
                      :initform (make-hash-table :test 'equal)
                      :type hash-table
                      :documentation "Hash-table of all watched
                      directories, if RECURSIVE-P is NIL this
                      Hash-table will only contain the DIR handle. If
                      RECURSIVE-P is T it will contain updated in case
                      a directory is added or removed. Do not set
                      member by Hand, these will be updated by
                      ADD-DIRECTORY-TO-WATCH and
                      REMOVE-DIRECTORY-FROM-WATCH, for more info see
                      CALLBACK.")
   (alive-p :reader alive-p
            :initform nil
            :type boolean
            :documentation "To check if watcher is alive and
            running.")
   (recursive-p :reader recursive-p
                :initarg :recursive-p
                :initform nil
                :type boolean
                :documentation "If T all subdirectories of DIR will be
                watched too, if NIL just DIR is watched.")
   (error-cb :reader error-cb
             :initarg :error-cb
             :initform nil
             :type function
             :documentation "Callback which gets called when a error
             is thrown. If unset the error wont be catched. Takes a
             single arugment, the error condition. Its set as the
             *default-event-handler*. Checkout
             http://orthecreedence.github.io/cl-async/event-handling#application-error-handling
             for more information. This callback also gets called if a
             error occures by calling hook.")))

(defun escape-wildcards (thing &optional (escape-char #\\))
  "Got the inspiration for that code from
  sb-impl::unparse-physical-piece, credits go to Xach for finding it.
  Thanks again for the helping me out"
  (let* ((srclen (length thing))
         (dstlen srclen))
    (dotimes (i srclen)
      (let ((char (char thing i)))
        (case char
          ((#\* #\? #\[)
           (incf dstlen))
          (t (when (char= char escape-char)
               (incf dstlen))))))
    (let ((result (make-string dstlen))
          (dst 0))
      (dotimes (src srclen)
        (let ((char (char thing src)))
          (case char
            ((#\* #\? #\[)
             (setf (char result dst) escape-char)
             (incf dst))
            (t (when (char= char escape-char)
                 (setf (char result dst) escape-char)
                 (incf dst))))
          (setf (char result dst) char)
          (incf dst)))
      result)))

(defun escaped-directory-exists-p (directory)
  (uiop:directory-exists-p
   (escape-wildcards directory)))

(defun escaped-file-exists-p (file)
  (uiop:file-exists-p
   (escape-wildcards file)))

(defun escaped-directory-files (directory &rest args)
  (apply #'uiop:directory-files
         (escape-wildcards directory)
         args))

(defun escaped-subdirectories (directory)
  (uiop:subdirectories
   (escape-wildcards directory)))

(defun get-event-type (filename renamed-p changed-p)
  "Will determine the Event-Type by using ESCAPED-DIRECTORY-EXISTS-P
   and ESCAPED-FILE-EXISTS-P. Will return one of the following types:
   :file-added, :file-removed, :file-changed, :directory-added.  Since
   its not possible to determine :directory-removed and :on-delete a
   :file-removed will be returned instead."
  (let ((file-exists-p (escaped-file-exists-p filename))
        (directory-exists-p (escaped-directory-exists-p filename)))
    (cond ((and renamed-p
                (not changed-p)
                file-exists-p
                (not directory-exists-p))
           :file-added)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                (not directory-exists-p))
           :file-removed)
          ((and (not renamed-p)
                changed-p
                file-exists-p
                (not directory-exists-p))
           :file-changed)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                directory-exists-p)
           :directory-added)
          ;; i dont know what exactly it means to get a renamed and
          ;; changed event on a directory, but it happens when 'touch'
          ;; is run on a directory. Guess we could just ignore it.
          ((and renamed-p
                changed-p
                (not file-exists-p)
                directory-exists-p)
           nil)
          ;; if we get a change event but the file is already gone
          ;; ignore it. It should be fine since a file-removed event
          ;; will follow.
          ((and (not renamed-p)
                changed-p
                (not file-exists-p)
                (not directory-exists-p))
           nil)
          (t
           (error (format nil
                          "Could not determine event type in GET-EVENT-TYPE, file: ~a~%~
                          (file-exists-p: ~a, directory-exists-p: ~a, renamed-p: ~a, changed-p: ~a)"
                          filename
                          file-exists-p directory-exists-p renamed-p changed-p))))))

(defun add-dir (watcher dir)
  "adds the specified dir to watcher, this function has to be called
   from the watcher-thread! See also: ADD-DIRECTORY-TO-WATCH."
  (when (or (recursive-p watcher)
            (string= (dir watcher) dir))
    (let ((table (directory-handles watcher)))
      (multiple-value-bind (value present-p) (gethash dir table)
        (declare (ignore value))
        (unless present-p
          (let ((handle (as:fs-watch dir
                                     (lambda (h f e s)
                                       (callback watcher h f e s)))))
            (setf (gethash dir table) handle)
            ;; this is _not_ nice... but there is no way to tell if we
            ;; attached the handler fast enough. Since the OS could have
            ;; already put some files inside the folder before we
            ;; attached the handler. To (somewhat) fix that this will
            ;; throw :file-created callbacks for each file, already
            ;; inside the directory. The ugly part is that this will
            ;; likely create duplicated :file-created events, since
            ;; files could have been created while the handler was
            ;; attached, but before this dolist finishes. But at least
            ;; this will catch all files.
            (dolist (sub-file (mapcar #'uiop:native-namestring
                                      (escaped-directory-files dir)))
              (callback watcher handle (subseq sub-file
                                               (length (get-handle-path handle)))
                        t
                        nil))
            ;; this makes sure that we dont miss any added directory
            ;; events. In case ADD-DIR is called with a sub-directoy
            ;; (from a filesystem event callback) which we already added
            ;; by iterating over all sub-directories, ADD-DIR will
            ;; return.
            (when (recursive-p watcher)
              (dolist (sub-dir (mapcar #'uiop:native-namestring
                                       (escaped-subdirectories dir)))
                (add-dir watcher sub-dir)))))))))

(defun add-directory-to-watch (watcher dir)
  "adds dir to watcher, can be safetly called by any thread, will
   interrupt watcher-thread if BT:CURRENT-THREAD != (THREAD WATCHER)."
  (when (pathnamep dir)
    (format t "ERROR: add-directory-to-watch: dir is pathnamep, this should not happen!~%")
    (setf dir (format nil "~a" dir)))
  (unless (char= #\/ (aref dir (- (length dir) 1)))
    (format t "ERROR: add-directory-to-watch: dir had no trailing /~%")
    (setf dir (concatenate 'string dir "/")))
  (if (eql (bt:current-thread)
           (thread watcher))
      (add-dir watcher dir)
      (bt:interrupt-thread (thread watcher)
                           #'add-dir watcher dir)))

(defun remove-directory-from-watch (watcher dir)
  "removes dir from watcher, can be safetly called by any thread, will
   interrupt watcher-thread if BT:CURRENT-THREAD != (THREAD WATCHER)"
  (when (pathnamep dir)
    (format t "ERROR: remove-directory-to-watch: dir is pathnamep, this should not happen~%")
    (setf dir (format nil "~a" dir)))
  (let* ((table (directory-handles watcher))
         (handle (gethash dir table)))
    (when handle
      ;; only call fs-unwatch if there is a handle. (handles are NIL
      ;; if RECURSIVE-P is NIL)
      (as:fs-unwatch handle))
    (remhash dir table)))

(defun get-handle-path (handle)
  "gets the path (string) of the given cl-async fs-handle."
  (let ((buffer (cffi:foreign-alloc :char
                                    :initial-element 0
                                    :count 2048))
        (size (cffi:foreign-alloc :uint
                                  :initial-element 0))
        (result nil))
    (uv:uv-fs-event-getpath (as::fs-monitor-c handle)
                            buffer
                            size)
    (setf result (cffi:foreign-string-to-lisp buffer))
    (cffi:foreign-free buffer)
    (cffi:foreign-free size)
    result))

(defgeneric callback (watcher handle filename renamed-p changed-p)
  (:documentation "the main callback which gets called if a Event occures. This
   function will determine the event type and then call the hook
   function, if set."))

(defmethod callback :around ((watcher watcher) handle filename renamed-p changed-p)
  (with-slots (event-loop-busy-p) watcher
    (setf event-loop-busy-p t)
    (unwind-protect
         (call-next-method)
      (setf event-loop-busy-p nil))))

(defmethod callback ((watcher watcher) handle filename renamed-p changed-p)
  (let ((event-type nil)
        (full-filename (concatenate 'string
                                    (get-handle-path handle)
                                    filename)))
    (if (eql 0 (length filename))
        (if (equal (dir watcher)
                   full-filename)
            ;; main directory got deleted
            (setf event-type :on-deleted)
            ;; in case it was not the main directory a subdirectory
            ;; was removed, so ignore it. Because this function will
            ;; be called again by the Handle from the Parent
            ;; Directory.
            (return-from callback))
        ;; some other event besides :on-deleted and :directory-removed
        (setf event-type (get-event-type full-filename renamed-p changed-p)))
    ;; if event-type is nil, it could not be determined and we ignore
    ;; the callback
    (unless event-type
      (return-from callback))
    ;; lets check if a directory was removed, just add a trailing /
    ;; and see if its inside directory-handles
    (when (eql event-type :file-removed)
      (let ((dir-name (concatenate 'string full-filename "/")))
        (multiple-value-bind (value present-p)
            (gethash dir-name (directory-handles watcher))
          (declare (ignore value))
          (when present-p
            (setf event-type :directory-removed)))))
    ;; in case a directoy was added/removed add a trailing /
    (when (or (eql event-type :directory-added)
              (eql event-type :directory-removed))
      (setf full-filename (concatenate 'string full-filename "/")))
    ;; add/remove directory from watcher
    (case event-type
      (:directory-added
       (add-directory-to-watch watcher full-filename))
      (:directory-removed
       (remove-directory-from-watch watcher full-filename)))
    ;; lets check if hook is set, and if so call it
    (let ((fn (hook watcher)))
      (when fn
        (lparallel.queue:push-queue
         (list fn watcher full-filename event-type)
         (slot-value watcher 'hook-queue))))
    (when (eql event-type :on-deleted)
      (stop-watcher watcher))))

(defun watcher-event-loop (watcher)
  "Watcher event loop, will be called by the watcher thread. This
   Function/Thread will return if all handles are removed. That will
   only happen if STOP-WATCHER is called or the Main Directory gets
   deleted. This thread will get interrupted by
   add-directory-to-watch-dir if a new directory is added."
  (as:with-event-loop (:catch-app-errors (error-cb watcher))
    (add-dir watcher (dir watcher))
    (setf (slot-value watcher 'alive-p) t)))

;; overwrite constructor and set DIR to a absolute Path, also start
;; the event-loop Thread
(defmethod initialize-instance :after ((watcher watcher) &rest initargs)
  ;; get fullpath as string and check if something went wrong
  (declare (ignorable initargs))
  (with-slots (dir) watcher
    (let ((fullpath (car (directory (escape-wildcards dir)))))
      (if fullpath
          (setf dir (uiop:native-namestring fullpath))
          (error "ERROR: The given Directory does not exist (or cannot~
                 be opened, no read/execute rights for example). calling~
                 (DIRECTORY ~a) returned NIL."
                 dir)))))

(defun hook-thread-main-loop (watcher)
  "Main Function of hook-thread. If watcher gets started, the
  hook-thread will call this function."
  (with-slots (hook-queue error-cb hook-busy-p skip-duplicated)
      watcher
    (loop :for event = (lparallel.queue:pop-queue hook-queue)
          :while (not (eql event :stop))
          :do (destructuring-bind (hook watcher filename event-type)
                  event
                (let ((skip nil))
                  (when skip-duplicated
                    (let ((next (lparallel.queue:peek-queue hook-queue)))
                      (unless (or (not next)
                                  (eql next :stop))
                        (destructuring-bind (nil nil n-filename n-event-type)
                            next
                          (when (and (equal filename n-filename)
                                     (eql event-type n-event-type))
                            (setf skip t))))))
                  (unless skip
                    (setf hook-busy-p t)
                    (unwind-protect
                         (handler-case
                             (funcall hook watcher filename event-type)
                           (error (ev)
                             (if error-cb
                                 (funcall error-cb ev)
                                 (error ev))))
                      (setf hook-busy-p nil))))))))

(defun start-watcher (watcher &optional thread-local-bindings)
  "starts the given watcher (starts watcher-event-loop thread and
  hook-thread). THREAD-LOCAL-BINDINGS is a alist of bindings which
  will be set via PROGV inside the threads."
  (with-slots (thread hook-thread dir) watcher
    (let ((symbols (map 'list #'car thread-local-bindings))
          (values (map 'list #'cdr thread-local-bindings)))
      (setf thread
            (bt:make-thread
             (lambda ()
               (progv symbols values
                 (watcher-event-loop watcher)))
             :name (format nil "cl-fs-watcher:event-loop ~a" dir)))
      (setf hook-thread
            (bt:make-thread
             (lambda ()
               (progv symbols values
                 (hook-thread-main-loop watcher)))
             :name (format nil "cl-fs-watcher:hook-thread ~a" dir))))))

(defun stop-watcher (watcher)
  "Will stop the Watcher. Removes all handles and joins the watcher
   thread. The given WATCHER can be deleted when STOP-WATCHER
   returns."
  (with-slots (thread hook-thread hook-queue alive-p directory-handles)
      watcher
    ;; unwatch all handles to stop event-loop-thread
    (loop :for path :being :the :hash-key :of directory-handles
          :do (progn
                (let ((handle (gethash path directory-handles)))
                  (when handle
                    (as:fs-unwatch handle)))
                (remhash path directory-handles)))
    ;; push :stop keyword onto hook-queue to stop hook-thread
    (lparallel.queue:push-queue :stop hook-queue)
    (setf alive-p nil)))

(defun get-all-tracked-files (watcher)
  "returns all files (excluding directories) which are tracked by the
   given watcher"
  (apply #'append
         (loop
           :for key :being :the :hash-keys :of (directory-handles watcher)
           :using (hash-value value)
           :if value
           :collect key)))

(defun busy-p (watcher)
  "Returns t if Watcher is 'busy' and there are items on the
  hook-queue. nil if hook-queue is empty and watcher is 'idle'."
  ;; (funcall (slot-value watcher 'dump))
  (with-slots (hook-busy-p event-loop-busy-p alive-p hook-queue thread)
      watcher
    (if (or hook-busy-p
            event-loop-busy-p
            (and (not alive-p) (bt:thread-alive-p thread))
            (not (lparallel.queue:queue-empty-p hook-queue)))
        t
        nil)))
