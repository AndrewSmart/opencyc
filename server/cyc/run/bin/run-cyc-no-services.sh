#!/bin/sh
#
# $Id: run-cyc-no-services.sh 128878 2009-10-02 17:10:33Z tbrussea $
# Assumes that java 1.6 or higher is in your path

#Set AGENT_LIB_PORT to 0 in order to disable remote debugging
AGENT_LIB_PORT=8886
MAIN_CLASS=com.cyc.tool.subl.jrtl.nativeCode.subLisp.SubLMain
#Set INIT_FORM to "(progn)" in order to disable initializations
#INIT_FORM="(progn)"
INIT_FORM="(progn (load \"init/jrtl-release-init.lisp\"))"
BASEDIR=`dirname $0`
$BASEDIR/cyc-runner.sh "${INIT_FORM}" "${AGENT_LIB_PORT}" "${MAIN_CLASS}" "$@"
