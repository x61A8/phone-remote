(defsystem "phone-remote"
  :depends-on ("cffi" "win32" "hunchentoot" "hunchensocket" "cl-qrencode")
  :serial t
  :components ((:file "packages")
	       (:file "phone-remote")))
