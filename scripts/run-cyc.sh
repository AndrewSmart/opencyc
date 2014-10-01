#!/usr/bin/env bash
#
# run-cyc.sh
#
# Shell script to execute the cyc server
#
# After some time loading the world into memory you will see CYC(1):
# which is the SubL command prompt.
# [optional] 
# You can enter SubL expressions such as (+ 1 2) or (genls #$Person)
# or (all-genls #$Person) at the command line to verify Cyc's operation.
# 
# At this point the cyc http server is running and you can access
# Cyc directly via the local web browser.
# http://localhost:3602/cgi-bin/cyccgi/cg?cb-start
# You can browse cyc via the Guest account or perform updates by
# logging on as CycAdminstrator.
#
# Modified work @ 2014 Andrew Smart

OPENCYC_RELEASE=opencyc-4.0


case $(pwd) in
   */${OPENCYC_RELEASE}) 
   cd server/cyc/run
   ;;
   */${OPENCYC_RELEASE}/scripts) 
   cd ../server/cyc/run
   ;;
  *)
   echo "Please run $0 from ${OPENCYC_RELEASE}/scripts"
   exit -1
esac

echo 'Launching CYC server at' $(date) '...'
bin/run-cyc.sh
echo 'Cyc server has shut down at' $(date)
exit 0
