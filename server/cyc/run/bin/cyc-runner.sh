#!/usr/bin/env bash
#
# $Id: cyc-runner.sh 140198 2012-05-29 20:57:21Z builder $
# Modified work @ 2014 Andrew Smart
# Assumes that java 1.6 or higher is in your path

##############################################################


java -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
    echo "Found a java executable."
else
    echo "Java is not available on the executable path."
    exit -1;
fi

INIT_FORM=$1
echo "Got init form: $INIT_FORM"

#set AGENT_LIB_PORT to 0 in order to disable remote debugging
AGENT_LIB_PORT=$2
echo "Got remote debugging port: $AGENT_LIB_PORT"

MAIN_CLASS=$3
echo "Got main class: ${MAIN_CLASS}"

shift
shift
shift

too_small_for_cyc () {
    echo 'ERROR: Cyc requires 64-bit Java to load this version of the Cyc KB.'
    echo 'The Java that is currently in your PATH is a 32-bit Java.'
    exit -1
}

#edit the following line to add your own JVM options
EXTRA_OPTIONS=

##############################################################

# ignore spaces in directory names above the CYC install directory
# for the purposes of this check
BASENAME=$(basename $(pwd | tr ' ' '_'))

if [ ${BASENAME} == "bin" ]; then
  # we are inside the bin directory ... moving one level up quietly
  cd ..
else 
  if [ -d "${PWD}/lib" ]; then
     # this is the correct location, we are good
     :
  else
     echo "I do not think that CYC is installed here."
     exit -1
  fi
fi

#----------------------------
# @section Logging Support
#----------------------------

# the logging facade's Jar
LOG_FACADE_JAR=lib/slf4j-api-1.6.1.jar

# the logging facade-to-framework bridge JAR and the framework's JAR
# an example using Log4J is provided here:
# LOG_FRAMEWORK="lib/slf4j-log4j12-1.6.1.jar lib/log4j-1.2.14.jar"
LOG_FRAMEWORK_JARS= # add your framework's elements here

if test -f ${LOG_FACADE_JAR}; then
   echo 'Logging is available ....'
   if test -z "${LOG_FRAMEWORK_JARS}"; then
      echo 'Warning ... logging is running with NO-OP framework.'
   fi 
   LOG_CPATH_ELEMENTS="${LOG_FACADE_JAR} ${LOG_FRAMEWORK_JARS}"
else
   echo 'Logging is not supported for this release ....'
   LOG_CPATH_ELEMENTS=
fi

# the logging settings that need to be passed to the executable
# this should be empty in releases without logging
# an example for Log4J is provided here
# LOG_FLAG='-Dlog4j.configuration=file:bin/cyc.log4j.properties -Dconfig.location=file -Dlog4j.debug=true'
LOG_FLAG=


#-----------------------------------
# @section Class Path Construction
#-----------------------------------

CLASSPATH=
CPATH_ELEMENTS="lib/cyc.jar lib/subl.jar lib/cycSparqlEndpoint.jar lib/commons-math-3.0-SNAPSHOT.jar lib/ext plugins"
JETTY_CPATH_ELEMENTS="lib/ant-1.6.5.jar lib/jetty-security-8.0.4.v20111024.jar lib/jetty-xml-8.0.4.v20111024.jar lib/ecj-3.5.1.jar lib/jetty-server-8.0.4.v20111024.jar lib/jsp-2.1-glassfish-2.1.v20100127.jar lib/jetty-continuation-8.0.4.v20111024.jar lib/jetty-servlet-8.0.4.v20111024.jar lib/jsp-api-2.1-glassfish-2.1.v20100127.jar lib/jetty-http-8.0.4.v20111024.jar lib/jetty-util-8.0.4.v20111024.jar lib/jetty-io-8.0.4.v20111024.jar lib/jetty-webapp-8.0.4.v20111024.jar lib/servlet-api-3.0.20100224.jar"


if test -n "${CYC_SCRIPT_SHOULD_FOLLOW_LINKS}"; then
    echo "Using canonicalized library paths."
else 
    echo "Not using canonicalized library paths."
fi

for pathElement in ${CPATH_ELEMENTS} ${LOG_CPATH_ELEMENTS} ${JETTY_CPATH_ELEMENTS}; do
    if test -n "${CYC_SCRIPT_SHOULD_FOLLOW_LINKS}"; then
        canonicalElement=`readlink -f ${pathElement}`
    else 
        canonicalElement="${pathElement}"
    fi
    if test -f ${canonicalElement} || test -d ${canonicalElement}; then
       if test -z "${CLASSPATH}"; then
          CLASSPATH=${canonicalElement}
       else
          CLASSPATH=${CLASSPATH}:${canonicalElement}
       fi
    else 
       echo 'Warning: Cannot find required class path element' ${canonicalElement} '....'
    fi
done

#echo CLASSPATH $CLASSPATH

_JAVA_OPTIONS=

BIT_FLAG= 
if [ `uname -s` = "SunOS" ]; then
  java -d64 -Xmx5000m -version  > /dev/null 2>&1;
  if [ $? -eq 0 ]; then
    echo "Running Solaris with 64 bit Java ...."
    BIT_FLAG=-d64
    MIN_HEAP=5000m
    if test -n "${CYC_MAX_HEAP}"; then
      MAX_HEAP=${CYC_MAX_HEAP}
    else
      MAX_HEAP=8000m
    fi;
    PERM_SIZE=256m
    CODE_CACHE_SIZE=96m
  else    # end 64-bit solaris
    # @note comment this out for OpenCYC
    too_small_for_cyc

  fi
else 
  java -Xmx5000m -version > /dev/null 2>&1;
  if [ 0 -eq $? ]; then
    echo "Running 64 bit Java ...."
    MIN_HEAP=5000m
    if test -n "${CYC_MAX_HEAP}"; then
      MAX_HEAP=${CYC_MAX_HEAP}
    else
      MAX_HEAP=8000m
    fi;
    PERM_SIZE=256m
    CODE_CACHE_SIZE=96m
  else
    too_small_for_cyc
  fi
fi

SERVER_FLAG= 
java -server -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  SERVER_FLAG=-server
  echo "Running with server flag ..."
else
  echo "Running without server flag ..."
fi

EA_FLAG= 
java -XX:+DoEscapeAnalysis -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  EA_FLAG=-XX:+DoEscapeAnalysis
  echo "Running with escape analysis ..."
else
  echo "Running without escape analysis ..."
fi

CM_FLAG= 
java -XX:+UseCompressedOops -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  CM_FLAG=-XX:+UseCompressedOops
  echo "Running with compressed memory ..."
else
  echo "Running without compressed memory ..."
fi

PGC_FLAG= 
java -XX:+UseParallelGC -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  PGC_FLAG=-XX:+UseParallelGC
  echo "Running with parallel garbage collector ..."
else
  echo "Running without parallel garbage collector ..."
fi

PERM_SIZE_FLAG=
java -XX:MaxPermSize=$PERM_SIZE -version > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  PERM_SIZE_FLAG=-XX:MaxPermSize=$PERM_SIZE
  echo "Running with increased permanent size ..."
else
  echo "Running without increased permanent size ..."
fi

CODE_CACHE_FLAG=
java -XX:ReservedCodeCacheSize=$CODE_CACHE_SIZE -version > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  CODE_CACHE_FLAG=-XX:ReservedCodeCacheSize=$CODE_CACHE_SIZE
  echo "Running with increased code cache ..."
else
  echo "Running without increased code cache ..."
fi

FAST_OPTS_FLAG= 
java -XX:+AggressiveOpts -XX:+UseBiasedLocking -XX:+UseFastAccessorMethods -XX:-UseSpinning  -XX:PreBlockSpin=10 -version  > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  FAST_OPTS_FLAG="-XX:+AggressiveOpts -XX:+UseBiasedLocking -XX:+UseFastAccessorMethods -XX:-UseSpinning  -XX:PreBlockSpin=10"
  echo "Running with fast optimizations ..."
else
  echo "Running without fast optimizations ..."
fi

AGENT_LIB_FLAG= 
java -agentlib:jdwp=transport=dt_socket,server=y,address=$AGENT_LIB_PORT,suspend=n -version > /dev/null 2>&1;
if [ $? -eq 0 ]; then
  if [ $AGENT_LIB_PORT -eq 0 ]; then
    echo "Running without remote debugging enabled ..."
  else
    AGENT_LIB_FLAG=-agentlib:jdwp=transport=dt_socket,server=y,address=$AGENT_LIB_PORT,suspend=n
    echo "Running with remote debugging enabled ..."
  fi
else
  echo "Running without remote debugging enabled ..."
fi

# enable and disable Java assert statements on parts of
# the Cyc code base, if the release supports it
# an example use for enabling assertions for a specific class
# ASSERTS_FLAG=-ea:com.cyc.cycjava.cycl.lexicon_utilities
ASSERTS_FLAG=

# apply XML based system parameter customization, 
# if the release supports it
CUSTOM_PARAM_JAR=lib/customSysParam.jar
if test -f ${CUSTOM_PARAM_JAR}; then
   echo 'Release supports XML-based system parameters.'
   CUSTOM_PARAM_XML=init/systemParameters.xml
   DEFAULT_PARAMS=init/parameters.lisp
   if test -f ${CUSTOM_PARAM_XML}; then
      echo "Applying custom system parameters in" ${CUSTOM_PARAM_XML} "to" ${DEFAULT_PARAMS} "...."
      java -jar ${CUSTOM_PARAM_JAR} ${CUSTOM_PARAM_XML} ${DEFAULT_PARAMS}
   else
      echo 'Using' ${DEFAULT_PARAMS} 'as is.'
   fi
fi

# Ready to start CYC
echo "Starting Cyc with command"
echo "java ${BIT_FLAG} ${SERVER_FLAG} -Xms${MIN_HEAP} -Xmx${MAX_HEAP} ${CODE_CACHE_FLAG} ${PERM_SIZE_FLAG} \
${EA_FLAG} ${CM_FLAG} ${PGC_FLAG} ${FAST_OPTS_FLAG} ${AGENT_LIB_FLAG} ${EXTRA_OPTIONS} \
${LOG_FLAG} ${ASSERTS_FLAG} -cp ${CLASSPATH} ${MAIN_CLASS} -f ${INIT_FORM} $@"

java ${BIT_FLAG} ${SERVER_FLAG} -Xms${MIN_HEAP} -Xmx${MAX_HEAP} ${CODE_CACHE_FLAG} ${PERM_SIZE_FLAG} \
${EA_FLAG} ${CM_FLAG} ${PGC_FLAG} ${FAST_OPTS_FLAG} ${AGENT_LIB_FLAG} ${EXTRA_OPTIONS} \
${LOG_FLAG} ${ASSERTS_FLAG} -cp "${CLASSPATH}" ${MAIN_CLASS} -f "${INIT_FORM}" "$@"
