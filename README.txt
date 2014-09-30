=====================================================
  Installation instructions for OpenCyc release 4.0
=====================================================

Welcome!

Two OpenCyc 4.0 releases are available, one for Linux and one for
Windows. (Since OpenCyc 4.0 is implemented in Java 1.6, the two
installations differ primarily in their startup scripts.)

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

1. Create a directory to contain opencyc (call it something like
   'opencyc'), copy the archive into that directory, then untar or
   unzip the compressed archive.

   For Linux:
  
       tar -xvfz opencyc-4.0.tgz

   For Windows:

       unzip the zipped folder

   This will create a directory named opencyc-4.0 under the directory
   you created.

2. Invoke the script to run OpenCyc. 

   For Linux: 

       cd opencyc-4.0/scripts
       ./run-cyc.sh

   For Windows: 

       Navigate to the directory opencyc-4.0\scripts
       double-click the file run-cyc.bat

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

