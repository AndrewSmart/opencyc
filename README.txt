### Synopsis:
OpenCyc 4.0 works on Linux and Windows- MacOS may require startup script changes. OpenCyc 4.0 is implemented in Java 1.6.

OpenCyc requires 3G RAM under 64bit and about 1G of disk space.
Running OpenCyc under 32bit is no longer supported.

When OpenCyc is started, it loads the KB into memory; depending on the amount of RAM on your system and the speed of your disk, this can take up to one or two minutes.

Once OpenCyc is running, you will see the SubL command prompt. Don't close the window containing this prompt because it will kill the image.
```
CYC(1):
```
* You can enter SubL expressions such as `(+ 1 2)` or `(genls #$Person)` or `(all-genls #$Person)` at the command line to verify Cyc's operation.

* At this point the Cyc http server is running and you can access Cyc directly via the local web browser. http://localhost:3602/cgi-bin/cg?cb-start

* To access from a remote machine, change 'localhost' to the IP name of the machine running Cyc, e.g. http://hamlet.mydomain.com:3602/cgi-bin/cg?cb-start

* You can browse Cyc via the Guest account or perform updates by logging on as CycAdministrator (the default).

You can cleanly shut down the OpenCyc server by entering:

```CYC(N): (halt-cyc-image)```

### Fork Notice:

The official release of OpenCyc is hosted here: https://sourceforge.net/projects/opencyc/

This fork of OpenCyc simply adds autocomplete to the web interface to significantly accelerate CycL input.

At this time ResearchCyc & EnterpriseCyc use the Cyc Core API: http://dev.cyc.com/api/core/download/ https://github.com/cycorp/api-suite
In the future the intention is for both the Core API and OpenCyc API to support OpenCyc 4.0. If all you intend to use/support is OpenCyc 4.0 then there is nothing to worry about.

Rather than store the large binary knowledge in git, Ant will download the large knowledge binaries, the OpenCyc API jar, and closed source jars from CycCorp's OpenCyc 4.0 release on sourceforge. The OpenCyc API source is here but the plumbing to build it from source was not set up, it is simply pulled from sourceforge along with the other jars. Issues may exist with it- for example Turkish characters are not supported. I apologize I didn't set up the plumbing to build the jar via Maven- but will welcome pull requests.

Enjoy! -Andrew R. Smart

