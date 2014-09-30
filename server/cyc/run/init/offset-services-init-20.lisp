;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

(in-package "CYC")

(csetq *gc-reports* nil)

(cvs-id "$Id: offset-services-init-20.lisp 126640 2008-12-04 13:39:36Z builder $")

(load "init/cyc-init.lisp")

;; determine the amount to offset services
(progn
  (csetq *base-tcp-port* 3620)
  (format t "Will offset TCP services to port ~A~%" *base-tcp-port*))

(load "init/port-init.lisp")


