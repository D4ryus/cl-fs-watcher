;;;; cl-fs-watcher.lisp

#|

This file contains the directory watcher which is used to watch a
directory for changes to its files and directories.

To use the Watcher create a instance of WATCHER with a directory to
watch and attach a hook. Once a Watcher is started two Threads are
created which handle the event-loop (see cl-async documentation for
more information about the event-loop) and hook callbacks. When the
event-loop emits an event it is added to the event-queue and taken by
the hook thread, which then calls the attached hook.

For example:

(defparameter *my-watcher*
              (make-instance 'cl-fs-watcher:watcher
                             :dir (pathname "~/watch-me/") ;; watch ~/watch-me/ ;
                             :hook (lambda (watcher pathname event-type) ;; call this function if anything happens
                                     (format t "something happend on watcher: ~a, which watches: ~a!~%"
                                             watcher (dir watcher))
                                     (format t "it happened to: ~a, event: ~a~%"
                                             pathname event-type))
                             :error-cb (lambda (ev)
                                         (format t "ERROR: ~a~%" ev)
                                         (cl-fs-watcher:stop-watcher *my-watcher*))))

(cl-fs-watcher:start-watcher *my-watcher*)

pathname is the absolute pathname to the changed file, so if
i-was-changed.txt inside ~/watch-me/some-dir/ is changed, the pathname
will be (if $HOME is /home/steve):
/home/steve/watch-me/some-dir/i-was-changed.txt (pathname will be a
pathname object)

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
        :type pathname
        :documentation "Main or Root Directory which will be watched,
        all its subdirectories will be watched too.")
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
   (queue :type lparallel.queue:queue
          :initform (lparallel.queue:make-queue)
          :documentation "Queue which is used to call functions from
          any thread inside the event-loop thread. Functions are
          pushed onto the queue and then a notifier inside the
          event-loop is triggered (see macro IN-EVENT-LOOP).")
   (queue-notifier :type cl-async:notifier
                   :initform nil
                   :documentation "Notifier which is fired when
                   functions are pushed to the QUEUE (see macro
                   IN-EVENT-LOOP).")
   (thread :reader thread
           :type bt:thread
           :initform nil
           :documentation "BT:THREAD which will run the event-loop, on
           start of the WATCHER the THREAD will be created. Thread
           will finish if DIR gets deleted or STOP-WATCHER is
           called.")
   (event-queue :reader event-queue
                :type lparallel.queue:queue
                :initform (lparallel.queue:make-queue)
                :documentation "Queue which holds all events in order,
                the event-loop will push events onto it and HOOK-THREAD
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
                :documentation "BT:THREAD which consumes events from
                the event-queue and calls the attached hook so that
                the event loop does not get blocked on longer running
                hooks.")
   (hook :accessor hook
         :initarg :hook
         :initform nil
         :type function
         :documentation "The function which gets called if a event
         occurs. HOOk needs to be a FUNCTION which takes 3
         arguments. It will be called with the Watcher Object, the
         pathname and the Event-type.

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

         If a subdirectory is created it will automatically be added
         to the watched list. If a subdirectory gets deleted the
         handle will be deleted too. So there is no need to handle
         those.

         Example:
         (setf (hook my-watcher-obj)
               (lambda (watcher pathname event-type)
                 (format t \"Hook from Watcher ~a was called!~%\"
                         watcher)
                 (format t \"File ~a, Event: ~a!~%\"
                         pathname event-type)))")
   (directory-handles :reader directory-handles
                      :initform (make-hash-table :test 'equal)
                      :type hash-table
                      :documentation "Hash-table of all watched
                      directories (Will only contain dir on windows
                      since windows automatically watches recursivly)
                      Do not set member by Hand, these will be updated
                      by ADD-DIRECTORY-TO-WATCH and
                      REMOVE-DIRECTORY-FROM-WATCH, for more info see
                      CALLBACK.")
   (alive-p :reader alive-p
            :initform nil
            :type boolean
            :documentation "To check if watcher is alive and
            running.")
   (error-cb :reader error-cb
             :initarg :error-cb
             :initform nil
             :type function
             :documentation "Callback which gets called when a error
             is thrown. If unset the error wont be catched. Takes a
             single argument, the error condition. Its set as the
             *default-event-handler*. Checkout
             http://orthecreedence.github.io/cl-async/event-handling#application-error-handling
             for more information. This callback also gets called if a
             error occures by calling hook.")))

;; macro which can be used to run code inside the event-loop
(defmacro in-event-loop ((watcher) &body body)
  `(progn
     (lparallel.queue:push-queue (lambda () ,@body)
                                 (slot-value ,watcher 'queue))
     (as:trigger-notifier (slot-value ,watcher 'queue-notifier))))

(defun escape-wildcards (thing &optional escape-char)
  "Got the inspiration for that code from
  sb-impl::unparse-physical-piece, credits go to Xach for finding it.
  Thanks again for the helping me out"
  (unless escape-char
    (setf escape-char
          #-os-windows #\\
          #+os-windows #\^))
  (let* ((srclen (length thing))
         (dstlen srclen))
    (dotimes (i srclen)
      (let ((char (char thing i)))
        (case char
          ((#\* #\? #\[ #+os-windows #\~)
           (incf dstlen))
          (t (when (char= char escape-char)
               (incf dstlen))))))
    (let ((result (make-string dstlen))
          (dst 0))
      (dotimes (src srclen)
        (let ((char (char thing src)))
          (case char
            ((#\* #\? #\[ #+os-windows #\~)
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
   (etypecase directory
     (pathname directory)
     (string (escape-wildcards directory)))))

(defun escaped-file-exists-p (file)
  (uiop:file-exists-p
   (etypecase file
     (pathname file)
     (string (escape-wildcards file)))))

(defun escaped-directory-files (directory &rest args)
  (apply #'uiop:directory-files
         (etypecase directory
           (pathname directory)
           (string (escape-wildcards directory)))
         args))

(defun escaped-subdirectories (directory)
  (uiop:subdirectories
   (etypecase directory
     (pathname directory)
     (string
      (escape-wildcards directory)))))

(defun get-event-type (pathname renamed-p changed-p)
  "Will determine the Event-Type by using ESCAPED-DIRECTORY-EXISTS-P
   and ESCAPED-FILE-EXISTS-P. Will return one of the following types:
   :file-added, :file-removed, :file-changed, :directory-added.  Since
   its not possible to determine :directory-removed and :on-delete a
   :file-removed will be returned instead. Returns nil if event-type
   is unknown"
  (let ((file-exists-p (escaped-file-exists-p pathname))
        (directory-exists-p (escaped-directory-exists-p pathname)))
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
          ;; this event occures on windows when something inside a
          ;; folder changes. We can ignore it since we get a fitting
          ;; event for the change that happend inside the directory
          #+os-windows
          ((and (not renamed-p)
                changed-p
                (not file-exists-p)
                directory-exists-p)
           nil)
          (t
           (error (format nil
                          "Could not determine event type in GET-EVENT-TYPE, file: ~a~%~
                          (file-exists-p: ~a, directory-exists-p: ~a, renamed-p: ~a, changed-p: ~a)"
                          pathname
                          file-exists-p directory-exists-p renamed-p changed-p))))))

(defun handle-sub-directories (watcher handle pathname)
  "calls callback to indicate a :file-created event for all files
inside the given folder (pathname). Will then iterate over all
sub-directories of pathname and call add-dir for each"
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
                            (escaped-directory-files pathname)))
    (callback watcher handle (subseq sub-file
                                     (length
                                      (uiop:native-namestring
                                       (get-handle-path handle))))
              t
              nil))
  ;; this makes sure that we dont miss any added directory
  ;; events. In case ADD-DIR is called with a sub-directoy
  ;; (from a filesystem event callback) which we already added
  ;; by iterating over all sub-directories, ADD-DIR will
  ;; return.
  (dolist (sub-dir (mapcar #'uiop:native-namestring
                           (escaped-subdirectories pathname)))
    (add-dir watcher (pathname (escape-wildcards sub-dir)))))

(defun add-dir (watcher pathname)
  "adds the specified pathname to watcher, this function has to be called
   inside the event-loop! See also: ADD-DIRECTORY-TO-WATCH."
  (let ((table (directory-handles watcher)))
    (multiple-value-bind (value present-p) (gethash pathname table)
      (declare (ignore value))
      (unless present-p
        (let ((handle #-os-windows (as:fs-watch (uiop:native-namestring pathname)
                                                (lambda (&rest args)
                                                  (apply #'callback watcher args)))
                      ;; on windows only add a handle when it's the root directory
                      #+os-windows (when (equal (dir watcher) pathname)
                                     (as:fs-watch (uiop:native-namestring pathname)
                                                  (lambda (&rest args)
                                                    (apply #'callback watcher args))))))
          ;; this will set the entry to nil on windows when pathname
          ;; is _not_ the root directory (since handle is nil)
          (setf (gethash pathname table) handle)
          (handle-sub-directories watcher
                                  #-os-windows handle
                                  ;; path root-dir handle to
                                  ;; handle-sub-directories so it can
                                  ;; construct the pathname (for the
                                  ;; sub-dirs and sub-files) with
                                  ;; get-handle-path and pathname
                                  #+os-windows (gethash (dir watcher) table)
                                  pathname))))))

(defun add-directory-to-watch (watcher pathname)
  "adds dir to watcher, can be safetly called by any thread, will
   use in-event-loop if BT:CURRENT-THREAD != (THREAD WATCHER)."
  (if (eql (bt:current-thread) (thread watcher))
      (add-dir watcher pathname)
      (in-event-loop (watcher)
        (add-dir watcher pathname))))

(defun remove-directory-from-watch (watcher pathname)
  "removes dir from watcher, can be safetly called by any thread, will
   use in-event-loop if BT:CURRENT-THREAD != (THREAD WATCHER)."
  (let ((table (directory-handles watcher)))
    (loop :for (remove-me . handle)
          ;; collect all subdirectory handles (we also get sub
          ;; directories of subdirectories)
          :in (loop :for file-pathname :being :the :hash-keys :of table
                    :using (hash-value handle)
                    :when (uiop:subpathp file-pathname pathname)
                    :collect (cons file-pathname handle))
          ;; remove them and if there is a handle unwatch it
          :do (progn
                (remhash remove-me table)
                (when handle
                  (if (eql (bt:current-thread)
                           (thread watcher))
                      (as:fs-unwatch handle)
                      (in-event-loop (watcher)
                        (as:fs-unwatch handle))))))))

(defun get-handle-path (handle)
  "gets the path (string) of the given cl-async fs-handle, returns a
  pathname object."
  (let ((buffer (cffi:foreign-alloc :char
                                    :initial-element 0
                                    :count 2048))
        (size (cffi:foreign-alloc :uint
                                  :initial-element 2048))
        (result nil))
    ;;(setf (cffi:mem-ref size :uint) 2048)
    (uv:uv-fs-event-getpath (as::fs-monitor-c handle)
                            buffer
                            size)
    (setf result (cffi:foreign-string-to-lisp buffer))
    (cffi:foreign-free buffer)
    (cffi:foreign-free size)
    (uiop:ensure-absolute-pathname
     (uiop:ensure-directory-pathname
      (escape-wildcards result)))))

(defgeneric callback (watcher handle namestring renamed-p changed-p)
  (:documentation "the main callback which gets called if a Event
  occures. This function will determine the event type and then add
  the event to the event-queue, when the hook function is set."))

(defmethod callback :around ((watcher watcher) handle namestring renamed-p changed-p)
  (with-slots (event-loop-busy-p) watcher
    (setf event-loop-busy-p t)
    (unwind-protect
         (call-next-method)
      (setf event-loop-busy-p nil))))

(defmethod callback ((watcher watcher) handle namestring renamed-p changed-p)
  (let ((event-type nil)
        (full-pathname (merge-pathnames (pathname (escape-wildcards namestring))
                                        (get-handle-path handle))))
    (setf event-type
          ;; namestring length of 0 means that some event on a
          ;; directory occured. Not 0 describes a file event
          (if (not (eql 0 (length namestring)))
              (get-event-type full-pathname renamed-p changed-p)
              ;; check if event occured on root directory (in case it
              ;; was removed), if not ignore it. Because this function
              ;; will be called again by the Handle from the Parent
              ;; Directory which namestring pointing to the changed
              ;; directory.
              (when (equal (dir watcher) full-pathname)
                ;; main directory got deleted
                #-os-windows :on-deleted
                ;; on windows we will catch main directory deletion by
                ;; checking if the directory still exists, since this
                ;; method does not work (we get frequent events on
                ;; directories when something inside them changes). So
                ;; if there is a event on the root-dir just ignore it.
                #+os-windows nil)))
    ;; if event-type is nil, it could not be determined or we want to
    ;; ignore it, just return.
    (unless event-type
      (return-from callback))
    ;; lets check if a directory was removed (since we get a
    ;; :file-removed event because we dont know if it was a
    ;; directory), convert to directory-pathname and check if we track
    ;; it
    (when (eql event-type :file-removed)
      (multiple-value-bind (value present-p)
          (gethash (uiop:ensure-directory-pathname full-pathname)
                   (directory-handles watcher))
        (declare (ignore value))
        (when present-p
          (setf event-type :directory-removed))))
    ;; in case a directoy was added/removed make sure we have a
    ;; directory pathname
    (when (or (eql event-type :directory-added)
              (eql event-type :directory-removed))
      (setf full-pathname (uiop:ensure-directory-pathname full-pathname)))
    ;; add/remove directory from watcher
    (case event-type
      (:directory-added
       (add-directory-to-watch watcher full-pathname))
      (:directory-removed
       (remove-directory-from-watch watcher full-pathname)))
    ;; if hook is set, add event to event-queue
    (let ((fn (hook watcher)))
      (when fn
        (lparallel.queue:push-queue
         (list fn watcher full-pathname event-type)
         (slot-value watcher 'event-queue))))
    ;; make sure watcher stops when the root-directory is
    ;; deleted
    (when (eql event-type :on-deleted)
      (stop-watcher watcher))))

(defun watcher-event-loop (watcher)
  "Watcher event loop, will be called by the watcher thread. This
   Function/Thread will return if all handles are removed. That will
   only happen if STOP-WATCHER is called or the Main Directory gets
   deleted."
  (as:with-event-loop (:catch-app-errors (error-cb watcher))
    (with-slots (dir alive-p queue-notifier queue) watcher
      (setf queue-notifier
            (cl-async:make-notifier
             (lambda ()
               (as:delay
                 (lambda ()
                   (loop :while (not (lparallel.queue:queue-empty-p queue))
                         :do (funcall (lparallel.queue:pop-queue queue))))))
             :single-shot nil))
      (add-dir watcher dir)
      (setf alive-p t)
      ;; there wont be a event on windows when the main directory gets
      ;; deleted, so we have to manually check if the tracked folder
      ;; still exists and if not stop the watcher by hand.
      #+os-windows
      (labels ((check-if-dir-deleted ()
                 (unless (escaped-directory-exists-p dir)
                   ;; call callback so that it will detect a on-deleted event
                   (callback watcher
                             (gethash (dir watcher) (directory-handles watcher))
                             ""
                             nil
                             nil))
                 (when alive-p
                   (as:delay #'check-if-dir-deleted
                     :time 1))))
        (check-if-dir-deleted)))))

(defmethod initialize-instance :after ((watcher watcher) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (dir) watcher
    (setf dir (uiop:ensure-absolute-pathname
               (uiop:ensure-directory-pathname
                dir)))
    (unless (escaped-directory-exists-p dir)
      (error "ERROR: The directory '~a' does not exist (or cannot~
             be opened, no read/execute rights for example)."
             dir))))

(defun hook-thread-main-loop (watcher)
  "Main Function of hook-thread. If watcher gets started, the
  hook-thread will call this function."
  (with-slots (event-queue error-cb hook-busy-p skip-duplicated)
      watcher
    (loop :for event = (lparallel.queue:pop-queue event-queue)
          :while (not (eql event :stop))
          :do (destructuring-bind (hook watcher filename event-type)
                  event
                (let ((skip nil))
                  (when skip-duplicated
                    (let ((next (lparallel.queue:peek-queue event-queue)))
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
  "Will stop the Watcher. Removes all handles and stops all threads."
  (with-slots (thread hook-thread event-queue alive-p directory-handles queue-notifier)
      watcher
    (when alive-p
      ;; unwatch all handles to stop event-loop-thread
      (loop :for path :being :the :hash-key :of directory-handles
            :do (remove-directory-from-watch watcher path))
      (in-event-loop (watcher)
        (as:free-notifier queue-notifier)
        (setf queue-notifier nil))
      ;; push :stop keyword onto event-queue to stop hook-thread
      (lparallel.queue:push-queue :stop event-queue)
      (setf alive-p nil))))

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
  event-queue. nil if event-queue is empty and watcher is 'idle'."
  (with-slots (hook-busy-p event-loop-busy-p alive-p event-queue thread)
      watcher
    (if (or hook-busy-p
            event-loop-busy-p
            (and (not alive-p) (bt:thread-alive-p thread))
            (not (lparallel.queue:queue-empty-p event-queue)))
        t
        nil)))
