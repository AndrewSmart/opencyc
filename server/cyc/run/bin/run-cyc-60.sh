#!/usr/bin/env sh
#
# $Id: run-cyc-60.sh 137116 2011-11-23 16:37:15Z sbrown $
# Modified work @ 2014 Andrew Smart
# Assumes that java 1.6 or higher is in your path

#Set AGENT_LIB_PORT to 0 in order to disable remote debugging
AGENT_LIB_PORT=8884
PORT_OFFSET=60
MAIN_CLASS=com.cyc.tool.subl.jrtl.nativeCode.subLisp.SubLMain
#Set INIT_FORM to "(progn)" in order to disable initializations
#INIT_FORM="(progn)"
INIT_FORM="(progn (load \"init/jrtl-release-init.lisp\") \
  (cinc *base-tcp-port* ${PORT_OFFSET}) \
  (load \"init/port-init.lisp\"))"
BASEDIR=`dirname $0`
$BASEDIR/cyc-runner.sh "${INIT_FORM}" "${AGENT_LIB_PORT}" "${MAIN_CLASS}" "$@"
