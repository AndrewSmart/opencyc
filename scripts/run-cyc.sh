#!/bin/bash
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
# Modified work @ 2014 Andrew Smart

scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${scriptdir}"

# Gets the binary OpenCyc data from official source.
# Gets binary libraries packaged with official OpenCyc distribution.
ant -f ant-get-binaries.xml -quiet complete

cd ../server/cyc/run

echo 'Launching CYC server at' $(date) '...'
bin/run-cyc.sh
echo 'Cyc server has shut down at' $(date)
exit 0
