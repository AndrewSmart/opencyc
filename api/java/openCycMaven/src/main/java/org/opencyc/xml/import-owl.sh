#!/usr/bin/env sh
#
# import-owl.sh
#
# Shell script to run the java application which imports a given OWL ontology
#
# usage:  $./import-owl.sh 
# for legacy API assertion mode:  $./import-owl.sh <anything e.g. foo>
#
# written by Stephen Reed  2/11/2004
# Modified work @ 2014 Andrew Smart
#
CLASSPATH=/cyc/java/lib/jakarta-oro-2.0.3.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/commons-collections.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/ViolinStrings.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/icu4j.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/jena.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/jug.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/UtilConcurrent.jar
CLASSPATH=$CLASSPATH:/cyc/java/lib/xerces.jar
CLASSPATH=$CLASSPATH:/home/reed/cvs/head/cycorp/cyc/java

cd /home/reed/cvs/head/cycorp/cyc/java

java -cp $CLASSPATH org.opencyc.xml.gui.ImportOwlApp $1

