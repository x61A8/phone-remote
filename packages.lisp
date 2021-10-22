(defpackage #:phone-remote
  (:use #:cl)
  (:import-from #:win32)
  (:import-from #:cffi)
  (:import-from #:hunchentoot)
  (:import-from #:hunchensocket)
  (:import-from #:cl-qrencode)
  (:export main)
  )
