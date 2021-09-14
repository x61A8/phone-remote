(defsystem "phone-remote"
  :depends-on ("cffi" "win32")
  :serial t
  :components ((:file "packages")
	       (:file "phone-remote")))
