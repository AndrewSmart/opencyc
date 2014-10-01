rem run-cyc.bat
rem
rem
rem Assumes that java 1.5 or higher (1.6 or higher preferably) is in your path

set CLASSPATH=lib\cyc.jar
set CLASSPATH=%CLASSPATH%;lib\subl.jar
set CLASSPATH=%CLASSPATH%;lib\junit.jar
set CLASSPATH=%CLASSPATH%;lib\cycSparqlEndpoint.jar
set CLASSPATH=%CLASSPATH%;resource
set CLASSPATH=%CLASSPATH%;lib\ext\
set CLASSPATH=%CLASSPATH%;plugins\
set CLASSPATH=%CLASSPATH%;lib\ant-1.6.5.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-security-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-xml-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\ecj-3.5.1.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-server-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jsp-2.1-glassfish-2.1.v20100127.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-continuation-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-servlet-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jsp-api-2.1-glassfish-2.1.v20100127.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-http-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-util-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-io-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\jetty-webapp-8.0.4.v20111024.jar
set CLASSPATH=%CLASSPATH%;lib\servlet-api-3.0.20100224.jar

set "BACKGROUND="

if "%1"=="-b" (
set "BACKGROUND=-b "
SHIFT
)
rem Use DEFAULT_PORT_OFFSET unless another offset was passed as the last 
rem argument this bat file

set DEFAULT_PORT_OFFSET=00

if "%1"==""  (
SET PORT_OFFSET=%DEFAULT_PORT_OFFSET%
goto :port_set
)

if  "%1"=="00" goto :set_port
if  "%1"=="20" goto :set_port
if  "%1"=="40" goto :set_port
if  "%1"=="60" goto :set_port
if  "%1"=="80" goto :set_port

rem AN INVALID PORT WAS SPECIFIED
echo ....ERROR Invalid port offset specified: %1%
if not defined BACKGROUND pause
exit

:set_port
SET PORT_OFFSET=%1

:port_set
echo PORT_OFFSET = %PORT_OFFSET%

set INIT_FORM="(progn (load \"init/jrtl-release-init.lisp\") (cinc *base-tcp-port* %PORT_OFFSET%) (load \"init/port-init.lisp\"))"

rem Add the following line to the JVM options to allow remote debugging
rem -agentlib:jdwp=transport=dt_socket,server=y,address=8888,suspend=n

java -server -Xms5g -Xmx8g -XX:MaxPermSize=256m -XX:+AggressiveOpts -XX:+UseParallelGC -XX:+UseBiasedLocking -XX:+UseFastAccessorMethods -XX:-UseSpinning -XX:PreBlockSpin=10 -XX:+UseCompressedOops -cp %CLASSPATH% com.cyc.tool.subl.jrtl.nativeCode.subLisp.SubLMain %BACKGROUND% -f %INIT_FORM%

if not defined BACKGROUND pause
