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


