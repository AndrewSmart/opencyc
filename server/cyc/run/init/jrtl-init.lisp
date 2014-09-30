;;; -*- Mode: LISP; Package: CYC; Syntax: ANSI-Common-Lisp -*-

(in-package "CYC")

(cvs-id "$Id: jrtl-init.lisp 132325 2010-09-28 16:53:34Z rck $")

(load-system-parameters)
(system-code-initializations)

(clet ((units-directory "units/5022/"))
  (pif (fboundp 'cyc-load-kb)
       (cyc-load-kb units-directory)
       (progn
         (load-kb units-directory)
         (system-code-initializations))))
