(in-package #:phone-remote)

;;; Keyboard Input
(defun init-scancodes ()
  (let ((ret (make-hash-table))
	(row1 "1234567890")
	(start1 #x02)
	(row2 "QWERTYUIOP")
	(start2 #x10)
	(row3 "ASDFGHJKL")
	(start3 #x1E)
	(row4 "ZXCVBNM")
	(start4 #x2C))
    (loop for char across row1
	  for scancode from start1
	  do (setf (gethash char ret) scancode))
    (loop for char across row2
	  for scancode from start2
	  do (setf (gethash char ret) scancode))
    (loop for char across row3
	  for scancode from start3
	  do (setf (gethash char ret) scancode))
    (loop for char across row4
	  for scancode from start4
	  do (setf (gethash char ret) scancode))
    ret))

(defparameter *scancodes* (init-scancodes))
(defun get-scancode (char)
  (gethash char *scancodes*))

(defun send-scancode-down (scancode)
  (cffi:with-foreign-object (inputs 'win32:input 1)
    (cffi:with-foreign-slots ((win32:type win32:input) (cffi:mem-aref inputs 'win32:input 0)  win32:input)
      (cffi:with-foreign-slots ((mi win32:ki hi) win32:input win32:input_input-union)
	(cffi:with-foreign-slots ((win32:vk win32:scan win32:flags time extra-info) win32:ki  win32:keybdinput)
	  (setf win32:type 1
		win32:vk 0
		win32:scan scancode
		win32:flags 8))))
    (win32:send-input 1 inputs (cffi:foreign-type-size 'win32:input))))

(defun send-scancode-up (scancode)
  (cffi:with-foreign-object (inputs 'win32:input 1)
    (cffi:with-foreign-slots ((win32:type win32:input) (cffi:mem-aref inputs 'win32:input 0)  win32:input)
      (cffi:with-foreign-slots ((mi win32:ki hi) win32:input win32:input_input-union)
	(cffi:with-foreign-slots ((win32:vk win32:scan win32:flags time extra-info) win32:ki  win32:keybdinput)
	  (setf win32:type 1
		win32:vk 0
		win32:scan scancode
		win32:flags 10))))
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
(defun strip-spaces (string)
  (flet ((spacep (char) (char= char #\Space)))
    (let ((start (position-if (complement #'spacep) string)))
      (if start
	  (let ((end (position-if (complement #'spacep) string :from-end t)))
	    (subseq string start (1+ end)))
	  ""))))

(defun key-line-p (line)
  (and (= (length line) 3)
       (char= (char line 1) #\Space)
       (alphanumericp (char line 2))))

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
	    do (process-line (string-upcase (strip-spaces line))))
      keymaps)))

;;; Websocket message handler
(defvar *keymaps* nil)
(defun handle-message (message)
  (funcall (if (char= (char message 0) #\D)
	       #'send-scancode-down
	       #'send-scancode-up)
	   (gethash (char message 1)
		    (aref *keymaps*
			  (parse-integer message :start 2)))))

;;; Websockets
(defclass ws-client (hunchensocket:websocket-client) ())
(defclass ws-server (hunchensocket:websocket-resource)
  ((path :initarg :path :initform (error "Assign me a path!") :reader path))
  (:default-initargs :client-class 'ws-client))

(defmethod hunchensocket:client-connected ((server ws-server) client)
  (format t "~&Client connected to websocket server.")
  (force-output))
(defmethod hunchensocket:client-disconnected ((server ws-server) client)
  (format t "~&Client disconnected from websocket server.")
  (force-output))
(defmethod hunchensocket:text-message-received ((server ws-server) client message)
  (handle-message message))  

(defvar *ws-server* nil)

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
  (setf *ws-server* (make-instance 'ws-server :path "/"))
  (let* ((web-server (start-server))
	 (ws-server (start-ws-server))
	 (target-url (create-target-url (read-host-address "address.txt")
					(hunchentoot:acceptor-port web-server)
					(hunchentoot:acceptor-port ws-server))))
    (format t "~&Created QR code @ ~A" (create-qr-code target-url))
    (format t "~&Ready @ ~A" target-url)
    target-url))
