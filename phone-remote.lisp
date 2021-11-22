(in-package #:phone-remote)

;;; Keyboard Input
(defun init-scancodes ()
  (let ((ret (make-hash-table))
	(scancode-assoc '((#x02 . "1234567890-=")
			  (#x10 . "QWERTYUIOP[]")
			  (#x1E . "ASDFGHJKL;'`")
			  (#x2C . "ZXCVBNM,./"))))
    (dolist (pair scancode-assoc ret)
      (loop for char across (cdr pair)
	    for scancode from (car pair)
	    do (setf (gethash char ret) scancode)))))

(defparameter *scancodes* (init-scancodes))
(defun get-scancode (char)
  (gethash char *scancodes*))

(defun send-scancode (scancode direction)
  "Direction is one of :up or :down."
  (cffi:with-foreign-object (inputs 'win32:input 1)
    (cffi:with-foreign-slots ((win32:type win32:input) (cffi:mem-aref inputs 'win32:input 0)  win32:input)
      (cffi:with-foreign-slots ((mi win32:ki hi) win32:input win32:input_input-union)
	(cffi:with-foreign-slots ((win32:vk win32:scan win32:flags time extra-info) win32:ki  win32:keybdinput)
	  (setf win32:type 1
		win32:vk 0
		win32:scan scancode
		win32:flags (logior 8 (ccase direction
					((:up) 2)
					((:down) 0)))))))
    (win32:send-input 1 inputs (cffi:foreign-type-size 'win32:input))))

;;; Config
(defun read-host-address (filespec)
  "Read the first line in filespec as the host address."
  (with-open-file (f filespec)
    (read-line f)))

;;; Webserver
(defun start-server ()
  (format t "~&Starting web server...")
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				    :document-root "./webpage/"
				    :port 0)))

;;; QR code
(defun create-target-url (address webpage-port ws-port)
  (format nil "http://~A:~A/?ws=~A" address webpage-port ws-port))

(defun create-qr-code (url)
  (format t "~&Creating QR code...")
  (cl-qrencode:encode-png url))

;;; Keymap interpreter
(defun key-line-p (line)
  (and (= (length line) 3)
       (char= (char line 1) #\Space)))

(defun player-line-p (line)
  (and (>= (length line) 2)
       (char= (char line 0) #\P)
       (every #'digit-char-p (subseq line 1))))

(defun blankp (line)
  (= (length line) 0))

(defun read-keymaps (stream)
  (let ((keymaps (make-array 0 :adjustable t :initial-element nil))
	(player nil))
    (flet ((process-line (line)
	     (cond ((key-line-p line) (when player
					(setf (gethash (char line 0) (aref keymaps player))
					      (get-scancode (char line 2)))))
		   ((blankp line) nil)
		   ((player-line-p line)
		    (setf player (parse-integer line :start 1))
		    (when (< (length keymaps) (+ player 1))
		      (setf keymaps (adjust-array keymaps (+ player 1) :initial-element nil)))
		    (unless (aref keymaps player)
		      (setf (aref keymaps player) (make-hash-table))))
		   (t (format t "~&Discarding invalid line: ~A~%" line)))))
      (loop as line = (read-line stream nil nil)
	    while line
	    do (process-line (string-upcase (string-trim " " line))))
      keymaps)))

;;; Websocket message handler
(defvar *keymaps* nil)
(defun handle-message (message)
  (send-scancode (gethash (char message 1)
			  (aref *keymaps*
				(parse-integer message :start 2)))
		 (if (char= (char message 0) #\D)
		     :down
		     :up)))

;;; Websockets
(defclass ws-client (hunchensocket:websocket-client) ())
(defclass ws-server (hunchensocket:websocket-resource)
  ((path :initarg :path :initform (error "Assign me a path!") :reader path))
  (:default-initargs :client-class 'ws-client))

(defmethod hunchensocket:client-connected ((server ws-server) client)
  (format t "~&Client connected to websocket server.~%")
  (force-output))
(defmethod hunchensocket:client-disconnected ((server ws-server) client)
  (format t "~&Client disconnected from websocket server.~%")
  (force-output))
(defmethod hunchensocket:text-message-received ((server ws-server) client message)
  (handle-message message))  

(defvar *ws-server* (make-instance 'ws-server :path "/"))

(defun find-ws-server (request)
  (when (string= (hunchentoot:script-name request) (path *ws-server*))
    *ws-server*))

(defun start-ws-server ()
  (format t "~&Starting WebSocket server...")
  (pushnew 'find-ws-server hunchensocket:*websocket-dispatch-table*)
  (hunchentoot:start (make-instance 'hunchensocket:websocket-acceptor :port 0)))

;;; Main
(defun main ()
  (format t "~&Reading keymaps...~%")
  (with-open-file (f "keymaps.txt")
    (setf *keymaps* (read-keymaps f)))
  (format t "~&Reading keymaps complete.~%")
  (let* ((web-server (start-server))
	 (ws-server (start-ws-server))
	 (target-url (create-target-url (read-host-address "address.txt")
					(hunchentoot:acceptor-port web-server)
					(hunchentoot:acceptor-port ws-server))))
    (format t "~&Created QR code @ ~A" (create-qr-code target-url))
    (format t "~&Ready @ ~A~%" target-url)
    (finish-output)
    (loop
      (sleep most-positive-fixnum))))
