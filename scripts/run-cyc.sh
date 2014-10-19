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
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${scriptdir}"

# See if ant is installed on system, if not then try to run cyc anyway.
if [ "$(command -v ant)" != "" ]; then
	# Gets the binary OpenCyc data from official source.
	# Gets binary libraries packaged with official OpenCyc distribution.
	ant -f ant-get-binaries.xml -quiet complete
else
	echo '!!!ant not found on system!!! Attempting to start OpenCyc anyway. If cyc fails to start, install ant so that this script can retrieve official OpenCyc binaries.'
fi

cd ../server/cyc/run

echo 'Launching CYC server at' $(date) ', type (halt-cyc-image) to quit...'
bin/run-cyc.sh
echo 'Cyc server has shut down at' $(date)
exit 0
