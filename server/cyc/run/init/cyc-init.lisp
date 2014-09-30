;;; -*- Package: CYC; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package "CYC")

(cvs-id "$Id: cyc-init.lisp 132323 2010-09-28 16:49:56Z rck $")

;;; This file should be the first thing loaded when a cyc
;;; image is started.

;; System parameters
(load-system-parameters)

;; CycL code initializations, includes KB-dependent initializations
(system-code-initializations)

;; chain to local definitions
(pwhen (probe-file "setup/my-cyc-init.lisp")
  (load "setup/my-cyc-init.lisp"))

;; this should be the very last step
(csetq *init-file-loaded?* t)
