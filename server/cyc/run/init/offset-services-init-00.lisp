;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

(in-package "CYC")

(csetq *gc-reports* nil)

(cvs-id "$Id: offset-services-init-00.lisp 127057 2009-02-09 19:50:06Z rck $")

(load "init/cyc-init.lisp")

;; determine the amount to offset services
(progn
  (csetq *base-tcp-port* 3600)
  (format t "Will offset TCP services to port ~A~%" *base-tcp-port*))

(load "init/port-init.lisp")


