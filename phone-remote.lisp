(in-package #:phone-remote)

;;; Keyboard Input
(defun init-vks ()
  (let ((ret (make-hash-table :size 36)))
    (loop for char from (char-code #\0) to (char-code #\9)
	  for vk from #x30 to #x39
	  do (setf (gethash (code-char char) ret) vk))
    (loop for char from (char-code #\A) to (char-code #\Z)
	  for vk from #x41 to #x5A
	  do (setf (gethash (code-char char) ret) vk))
    ret))

(defparameter *vks* (init-vks))
(defun get-vk (char)
  (gethash char *vks*))

(defun send-keydown (virtual-keycode)
  (cffi:with-foreign-object (inputs 'win32:input 1)
    (cffi:with-foreign-slots ((win32:type win32:input) (cffi:mem-aref inputs 'win32:input 0)  win32:input)
      (cffi:with-foreign-slots ((mi win32:ki hi) win32:input win32:input_input-union)
	(cffi:with-foreign-slots ((win32:vk scan flags time extra-info) win32:ki  win32:keybdinput)
	  (setf win32:type 1
		win32:vk virtual-keycode))))
    (win32:send-input 1 inputs (cffi:foreign-type-size 'win32:input))))

(defun send-keyup (virtual-keycode)
  (cffi:with-foreign-object (inputs 'win32:input 1)
    (cffi:with-foreign-slots ((win32:type win32:input) (cffi:mem-aref inputs 'win32:input 0) win32:input)
      (cffi:with-foreign-slots ((mi win32:ki hi) win32:input win32:input_input-union)
	(cffi:with-foreign-slots ((win32:vk scan win32:flags time extra-info) win32:ki win32:keybdinput)
	  (setf win32:type 1
		win32:vk virtual-keycode
		win32:flags 2))))
    (win32:send-input 1 inputs (cffi:foreign-type-size 'win32:input))))

;;; Config
(defun read-host-address (filespec)
  "Read the first line in filespec as the host address."
  (with-open-file (f filespec)
    (read-line f)))

;;; Webserver
(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				    :document-root "./webpage/"
				    :port 0)))

;;; QR code
(defun create-target-url (address webpage-port ws-port)
  (format nil "http://~A:~A/?ws=~A" address webpage-port ws-port))

(defun create-qr-code (url)
  (cl-qrencode:encode-png url))

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
  (format t "~&Websocket server recieved: | ~A |" message)
  (force-output))  


(defparameter *ws-server* (make-instance 'ws-server :path "/"))

(defun find-ws-server (request)
  (when (string= (hunchentoot:script-name request) (path *ws-server*))
    *ws-server*))

(defun start-ws-server ()
  (pushnew 'find-ws-server hunchensocket:*websocket-dispatch-table*)
  (hunchentoot:start (make-instance 'hunchensocket:websocket-acceptor :port 0)))

;;; Main
(defun main ()
  (let* ((web-server (start-server))
	 (ws-server (start-ws-server))
	 (target-url (create-target-url (read-host-address "address.txt")
					(hunchentoot:acceptor-port web-server)
					(hunchentoot:acceptor-port ws-server))))
    (create-qr-code target-url)
    target-url))
