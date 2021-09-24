(defsystem "phone-remote"
  :depends-on ("cffi" "win32" "hunchentoot" "cl-qrencode")
  :serial t
  :components ((:file "packages")
	       (:file "phone-remote")))
