;;; -*- Package: CYC; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package "CYC")

(cvs-id "$Id: port-init.lisp 138331 2012-01-27 21:48:26Z rck $")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Whoever modifies this file needs to also modify the Lisp
;;; version of the file, which is called 
;;;
;;;   /cyc/top/init/lisp-port-init.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define robust-enable-tcp-server (type port handler)
  (pif (fboundp 'enable-tcp-server)
       ;; new way
       (ret (enable-tcp-server type port))
       ;; old way
       (ret (sl::start-tcp-server port handler nil))))


;; Starts up the Cyc TCP servers, just as lisp-port-init.lisp does on
;; the Lisp side

;; *base-tcp-port* should be set
(punless (boundp '*base-tcp-port*)
  ;; use default of 3600 for C images
  (csetq *base-tcp-port* 3600))

;; Allow several Cyc images to run on the same computer by offsetting
;; the TCP ports to which services are bound.

(progn
  (format t "~&Enabling base TCP services to port ~S.~%" *base-tcp-port*)
  (finish-output))

;;; Calculate the explicit address for each port.
;;; Store the value in the necessary parameter.
;;; Start the server at that port.

;; HTML server
(pwhen (fboundp 'html-server-top-level)
  (robust-enable-tcp-server
   :html (html-port) 'html-server-top-level))
;; API server
(pwhen (fboundp 'api-server-top-level)
  (robust-enable-tcp-server
   :cyc-api (api-port) 'api-server-top-level))
;; CFASL server
(pwhen (fboundp 'cfasl-server-top-level)
  (robust-enable-tcp-server
   :cfasl (cfasl-port) 'cfasl-server-top-level))
;; HTTP server
(pwhen (fboundp 'http-server-top-level)
  (pwhen (robust-enable-tcp-server
          :http (http-port) 'http-server-top-level)
    (format t "HTTP server listening on port ~A.  Connect via URL http://~A:~A/cgi-bin/cg?cb-start~%"
            (http-port) (hostname) (http-port))))


;; Thesaurus API server
(pwhen-feature :Cyc-Thesaurus
  (pwhen *tm-start-tmap-on-startup?*
    (robust-enable-tcp-server
     :tmap (tmap-port)
     #'thesaurus-manager-access-protocol-server-top-level)))

(csetq *inference-trace-port* (+ *base-tcp-port* 7))

;; SPAQRL server endpoint -- only available in JRTL
(pwhen (fboundp 'start-external-sparql-process)
  (robust-enable-tcp-server
   :sparql (sparql-port)
   'start-external-sparql-process))

;; Servlet Container (aka jetty) -- only available in JRTL
(pwhen (fboundp 'start-servlet-container)
  (robust-enable-tcp-server
   :servlet (servlet-container-port)
   'start-servlet-container))

(punless-feature :Cyc-Alexandria
  (start-agenda))

(format t "~&Ready for services.~%")
