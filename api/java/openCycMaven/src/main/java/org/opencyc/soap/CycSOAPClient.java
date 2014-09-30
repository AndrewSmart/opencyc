package org.opencyc.soap;

import javax.xml.rpc.*;
import java.net.*;
import java.rmi.RemoteException;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.apache.axis.encoding.XMLType;
import org.opencyc.util.Log;

/**
 * Accesses the Cyc API as an XML SOAP client.<p>
 *
 * @version $Id: CycSOAPClient.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stephen L. Reed
 *
 * <p>Copyright 2001 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
public class CycSOAPClient {

    /**
     * Required jars
     *
     * common-collections.jar
     * jug.jar
     * jajarta-oro-2.0.3.jar
     * axis.jar
     * jaxrpc.jar
     * commons-discovery.jar
     * commons-logging.jar
     */

   /**
    * the web service URL
    */
   public String endpointURL = "http://localhost:8080/axis/services/CycSOAPService";
//   public String endpointURL = "http://207.207.8.29/axis/services/CycSOAPService";
    
    /**
     * Construct a new CycSOAPClient object.
     */
    public CycSOAPClient () {
    }

    /**
     * Provides the main method for the CycSOAPClient application.
     *
     * @param args the command line arguments
     */
    public static void main (String[] args) {
        Log.makeLog("Cyc-SOAP-client.log");
        CycSOAPClient cycSOAPClient = new CycSOAPClient();
        try {
            cycSOAPClient.helloWorld();
            for (int i = 0; i < 10; i++) {
                Log.current.print((i + 1) + ". ");
                Log.current.println(cycSOAPClient.remoteSubLInteractor("(isa #$TransportationDevice)"));
            }
            Log.current.println("categorizeEntity Service");
            String entityString = "Osama Bin Laden";
            String generalEntityKindString = "PERSON";
            Log.current.println("categorizeEntity(\"" + entityString + "\", \"" + generalEntityKindString + "\")");
            String response = cycSOAPClient.categorizeEntity(entityString, generalEntityKindString);
            Log.current.println("response=" + response);
            
            String ontologyString = "AXISConstant";
            Log.current.println("categorizeEntityWithinOntology Service");
            Log.current.println("categorizeEntityWithinOntology(\"" + entityString + "\", \"" + generalEntityKindString + "\", \"" + ontologyString + "\")");
            response = cycSOAPClient.categorizeEntityWithinOntology(entityString, generalEntityKindString, ontologyString);
            Log.current.println("response=" + response);
        }
        catch( Exception e ) {
            Log.current.errorPrintln(e.getMessage());
            Log.current.printStackTrace(e);
        }
    }

    /**
     * Provides a simple test of the SOAP service without Cyc access.
     */
    public void helloWorld ()
        throws ServiceException, MalformedURLException, RemoteException {
        String methodName = "getHelloWorldMessage";
        Service service = new Service();
        Call call = (Call) service.createCall();
        call.setTargetEndpointAddress(new java.net.URL(endpointURL));
        call.setOperationName(methodName);
        call.addParameter("echo",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.setReturnType(XMLType.XSD_STRING);
        String result = (String) call.invoke(new Object[] {"AXIS"});
        Log.current.println(result);
    }

    /**
     * Provides a SubL interactor client.
     *
     * @param subLRequest the given SubL request which will be submitted to the
     * Cyc server for evaluation
     * @return the result of evaluating the given SubL request
     */
    public String remoteSubLInteractor (String subLRequest)
        throws ServiceException, MalformedURLException, RemoteException {
        String methodName = "subLInteractor";
        Service service = new Service();
        Call call = (Call) service.createCall();
        call.setTargetEndpointAddress(new java.net.URL(endpointURL));
        call.setOperationName(methodName);
        call.addParameter("subLRequest",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.setReturnType(XMLType.XSD_STRING);
        return (String) call.invoke(new Object[] {subLRequest});
    }
    
    /**
     * Categorizes the given entity within the Cyc KB.
     *
     * @param entity the given entity to categorize
     * @param entityKind the given general entity kind as determined from information
     * extraction
     * @return an XML structure consisting of the mathched entity paraphrase and Cyc category, 
     * and if unmatched return an empty string
     */
    public String categorizeEntity  (String entityString, String generalEntityKindString)
        throws ServiceException, MalformedURLException, RemoteException {
        String methodName = "categorizeEntity";
        Service service = new Service();
        Call call = (Call) service.createCall();
        call.setTargetEndpointAddress(new java.net.URL(endpointURL));
        call.setOperationName(methodName);
        call.addParameter("entityString",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.addParameter("generalEntityKindString",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.setReturnType(XMLType.XSD_STRING);
        return (String) call.invoke(new Object[] {entityString, generalEntityKindString});
    }
    
    /**
     * Categorizes the given entity within the given ontology.
     *
     * @param entity the given entity to categorize
     * @param entityKind the given general entity kind as determined from information
     * extraction
     * @param ontologyString the given ontology (i.e. KBSubsetCollection term)
     * @return an XML structure consisting of the mathched entity paraphrase and Cyc category, 
     * and if unmatched return an empty string
     */
    public String categorizeEntityWithinOntology  (final String entityString, final String generalEntityKindString, final String ontologyString)
        throws ServiceException, MalformedURLException, RemoteException {
        String methodName = "categorizeEntityWithinOntology";
        Service service = new Service();
        Call call = (Call) service.createCall();
        call.setTargetEndpointAddress(new java.net.URL(endpointURL));
        call.setOperationName(methodName);
        call.addParameter("entityString",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.addParameter("generalEntityKindString",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.addParameter("ontologyString",
                          XMLType.XSD_STRING,
                          ParameterMode.IN);
        call.setReturnType(XMLType.XSD_STRING);
        return (String) call.invoke(new Object[] {entityString, generalEntityKindString, ontologyString});
    }
}



