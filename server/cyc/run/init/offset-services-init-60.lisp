;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

(in-package "CYC")

(cvs-id "$Id: offset-services-init-60.lisp 126640 2008-12-04 13:39:36Z builder $")

(csetq *gc-reports* nil)

(load "init/cyc-init.lisp")

;; determine the amount to offset services
(progn
  (csetq *base-tcp-port* 3660)
  (format t "Will offset TCP services to port ~A~%" *base-tcp-port*))

(load "init/port-init.lisp")

