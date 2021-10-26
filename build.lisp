(load "~/quicklisp/setup.lisp")
(ql:quickload "phone-remote")
(save-lisp-and-die "phone-remote.exe"
		   :toplevel #'phone-remote:main
		   :executable t
		   :purify t)
