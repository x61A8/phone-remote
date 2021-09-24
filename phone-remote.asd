(defsystem "phone-remote"
  :depends-on ("cffi" "win32" "hunchentoot")
  :serial t
  :components ((:file "packages")
	       (:file "phone-remote")))
