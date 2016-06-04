Fork Notice:

At this time ResearchCyc & EnterpriseCyc use the Cyc Core API: http://dev.cyc.com/api/core/download/ https://github.com/cycorp/api-suite
In the future the intention is for both the Core API and OpenCyc API to support OpenCyc 4.0. If all you intend to use/support is OpenCyc 4.0 then there is nothing to worry about.

This fork of OpenCyc simply adds autocomplete to the web interface to significantly accelerate CycL input.

Rather than store the large binary knowledge in git, Ant will download the large knowledge binaries, the OpenCyc API jar, and closed source jars from CycCorp's OpenCyc 4.0 release on sourceforge. The OpenCyc API source is here but the plumbing to build it from source was not set up, it is simply pulled from sourceforge along with the other jars. Issues may exist with it- for example Turkish characters are not supported. I apologize I didn't set up the plumbing to build the jar via Maven and I have no incentive to- but will welcome pull requests.

Enjoy! -Andrew R. Smart

=====================================================
  Installation instructions for OpenCyc release 4.0
=====================================================

Welcome!

OpenCyc 4.0 is available for Linux and Windows.
Since OpenCyc 4.0 is implemented in Java 1.6.

OpenCyc requires 3G RAM under 64bit and about 1G of disk space.
Running OpenCyc under 32bit is no longer supported.


OpenCyc for Windows has been tested on

  = Windows Server 2008 
  = Windows 7 Personal, Professional and Ultimate

OpenCyc for Linux has been tested on 

  = SuSE 10.3 and OpenSuSE 11
  = Ubuntu 12.04 Precise
  = Debian 6.04 


This OpenCyc release has not been tested on Mac OS-X or on Solaris.
However, other versions of Cyc are currently deployed on these
architectures and as little work as adjusting the startup scripts may
be all that is needed.


OpenCyc 4.0 has been tested with the following Java implementations

 (*) OpenJDK 1.6 IcedTea6 1.11.1
 (*) OpenJDK 1.7 IcedTea7  2.1.1
 (*) Oracle/Sun JDK 1.6 u32
 (*) Oracle/Sun JDK 1.7 u4
 (*) IBM Java 1.6 u10
 (*) IBM Java 1.7 u3


To view the full OpenCyc documentation: http://www.opencyc.org

== Installation ==

1. You need to get the source code from GitHub, from a command such as
   the following:
       git clone git@github.com:AndrewSmart/opencyc.git

   The author follows the git branching model mentioned here:
       http://nvie.com/posts/a-successful-git-branching-model/

   The repository doesn't contain any compiled .jar files; those are
   retrieved/compiled later by Ant/Maven.

2. Invoke the script to run OpenCyc. This script requires Ant to be
   installed on your system.

   For Linux: 

       cd opencyc-4.0/scripts
       ./run-cyc.sh

   For Windows: 

       Navigate to the directory opencyc-4.0\scripts
       double-click the file run-cyc.bat

   When the script is ran, it calls Ant in order to verify the official
   binaries (compiled .jar files) are installed, if they are not it will
   retrieve them from sourceforge.net.

   When OpenCyc is started, it loads the KB into memory; depending on the
   amount of RAM on your system and the speed of your disk, this can take
   between one and two minutes. 

   Once OpenCyc is running, you will see 

   CYC(1):

   which is the SubL command prompt. Don't close the window containing
   this prompt because it will kill the image.

3. (optional) 
   You can enter SubL expressions such as (+ 1 2) or (genls #$Person)
   or (all-genls #$Person) at the command line to verify Cyc's operation.
 
4. At this point the cyc http server is running and you can access
   Cyc directly via the local web browser.

   http://localhost:3602/cgi-bin/cg?cb-start

   To access from a remote machine, change 'localhost' to the IP name of the
   machine running Cyc, e.g.

   http://hamlet.mydomain.com:3602/cgi-bin/cg?cb-start

   You can browse Cyc via the Guest account or perform updates by
   logging on as CycAdministrator (the default).

5. You can cleanly shut down the OpenCyc server by entering (halt-cyc-image) 
   at the SubL command prompt, CYC(N):

