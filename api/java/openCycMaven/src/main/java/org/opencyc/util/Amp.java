package  org.opencyc.util;

/**
 * Provides the behavior and attributes of the Agent Manager Protocol - an agent
 * communication language (ACL) for OpenCyc.<p>
 *
 * @version $Id: Amp.java 138070 2012-01-10 19:46:08Z sbrown $
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

import  java.io.*;


public class Amp
        implements Serializable {
    /**
     * The AMP message in string form.
     */
    protected String ampString;

    /**
     * The AMP performative (command).
     */
    protected String performative;

    /**
     * The ID of the sender of the message.
     */
    protected String sender;

    /**
     * The ID of the receiver of the message.
     */
    protected String receiver;

    /**
     * The reference message ID to be used in a subsequent reply.
     */
    protected String replyWith;

    /**
     * The ID of the message for which this message is a reply.
     */
    protected String inReplyTo;

    /**
     * The content of the message.
     */
    protected String content;

    /**
     * The (computer) language in which the content is written.
     */
    protected String language;

    /**
     * The semantics defining the meaning of terms in the content.
     */
    protected String ontology;

    /**
     * Force indicator.
     */
    protected String force;

    /**
     * Constructs a AMP object given the Amp message string.
     *
     * @param ampString the Amp message string
     */
    public Amp (String ampString) {
        this.ampString = ampString;
        parsePerformativeParameters();
    }

    /**
     * Constructs an emply AMP message.
     */
    public Amp () {
        ampString = "";
        performative = "";
        sender = "";
        receiver = "";
        replyWith = "";
        inReplyTo = "";
        content = "";
        language = "";
        ontology = "";
        force = "";
    }

    /***
     * Returns the AMP message string.
     *
     * @return the AMP message string
     */
    public String toString () {
        if (ampString == "")
            generateAmpString();
        return  ampString;
    }

    /***
     * Sets the AMP string according to the existing values of
     * the performative parameters.
     */
    public void generateAmpString () {
        ampString = "(" + performative;
        if (!sender.equals(""))
            ampString = ampString + "\n  :SENDER " + sender;
        if (!receiver.equals(""))
            ampString = ampString + "\n  :RECEIVER " + receiver;
        if (!replyWith.equals(""))
            ampString = ampString + "\n  :REPLY-WITH " + replyWith;
        if (!inReplyTo.equals(""))
            ampString = ampString + "\n  :IN-REPLY-TO " + inReplyTo;
        if (!content.equals(""))
            ampString = ampString + "\n  :CONTENT " + content;
        if (!language.equals(""))
            ampString = ampString + "\n  :LANGUAGE " + language;
        if (!ontology.equals(""))
            ampString = ampString + "\n  :ONTOLOGY " + ontology;
        if (!force.equals(""))
            ampString = ampString + "\n  :FORCE " + force;
        ampString = ampString + ")";
    }

    /**
     * Returns the ampString.
     *
     * @return the ampString
     */
    public String ampString () {
        return  ampString;
    }

    /**
     * Returns the performative.
     *
     * @return the performative
     */
    public String performative () {
        return  performative;
    }

    /**
     * Sets the performative.
     *
     * @param performative the performative
     */
    public void setPerformative (String performative) {
        this.performative = performative.toUpperCase();
    }

    /**
     * Returns the sender.
     *
     * @return the sender
     */
    public String sender () {
        return  sender;
    }

    /**
     * Sets the sender.
     *
     * @param sender the sender
     */
    public void setSender (String sender) {
        this.sender = sender;
    }

    /**
     * Returns the receiver.
     *
     * @return the receiver
     */
    public String receiver () {
        return  receiver;
    }

    /**
     * Sets the receiver.
     *
     * @param receiver the receiver
     */
    public void setReceiver (String receiver) {
        this.receiver = receiver;
    }

    /**
     * Returns the replyWith.
     *
     * @return the replyWith
     */
    public String replyWith () {
        return  replyWith;
    }

    /**
     * Sets the replyWith.
     *
     * @param replyWith the replyWith
     */
    public void setReplyWith (String replyWith) {
        this.replyWith = replyWith;
    }

    /**
     * Returns the inReplyTo.
     *
     * @return the inReplyTo
     */
    public String inReplyTo () {
        return  inReplyTo;
    }

    /**
     * Sets the inReplyTo, wrapping it in quote delimiters.
     *
     * @param inReplyTo the inReplyTo
     */
    public void setInReplyToString (String inReplyTo) {
        this.inReplyTo = "\"" + inReplyTo + "\"";
    }

    /**
     * Sets the inReplyTo.
     *
     * @param inReplyTo the inReplyTo
     */
    public void setInReplyTo (String inReplyTo) {
        this.inReplyTo = inReplyTo;
    }

    /**
     * Returns the content.
     *
     * @return the content
     */
    public String content () {
        return  content;
    }

    /**
     * Sets the content.
     *
     * @param content the content
     */
    public void setContent (String content) {
        this.content = content;
    }

    /**
     * Sets the content, wrapping it in quote delimiters.
     *
     * @param content the content
     */
    public void setContentString (String content) {
        this.content = "\"" + content + "\"";
    }

    /**
     * Returns the language.
     *
     * @return the language
     */
    public String language () {
        return  language;
    }

    /**
     * Sets the language.
     *
     * @param language the language
     */
    public void setLanguage (String language) {
        this.language = language;
    }

    /**
     * Returns the ontology.
     *
     * @return the ontology
     */
    public String ontology () {
        return  ontology;
    }

    /**
     * Sets the ontology.
     *
     * @param ontology the ontology
     */
    public void setOntology (String ontology) {
        this.ontology = ontology;
    }

    /**
     * Returns the force.
     *
     * @return the force
     */
    public String force () {
        return  force;
    }

    /**
     * Sets the force.
     *
     * @param force the force
     */
    public void setForce (String force) {
        this.force = force;
    }

    /**
     * Creates a trivial AMP reply message.
     */
    public Amp createReply () {
        Amp replyMessage = new Amp();
        replyMessage.setSender(receiver);
        replyMessage.setReceiver(sender);
        replyMessage.setInReplyTo(content);
        replyMessage.setLanguage(language);
        replyMessage.setOntology(ontology);
        return  replyMessage;
    }

    /**
     * Constructs a AMP object using the current Amp message string.
     */
    public void parsePerformativeParameters () {
        performative = parsePerformative();
        sender = parseAmpPerformativeParameter(":SENDER");
        receiver = parseAmpPerformativeParameter(":RECEIVER");
        replyWith = parseAmpPerformativeParameter(":REPLY-WITH");
        inReplyTo = parseAmpPerformativeParameter(":IN-REPLY-TO");
        content = parseAmpPerformativeParameter(":CONTENT");
        language = parseAmpPerformativeParameter(":LANGUAGE");
        ontology = parseAmpPerformativeParameter(":ONTOLOGY");
        force = parseAmpPerformativeParameter(":FORCE");
    }

    /**
     * Returns the performative word in the AMP string.
     *
     * @return the performative word in the AMP string
     */
    public String parsePerformative () {
        int length = ampString.length();
        boolean isLeadingWhitespace = true;
        char ch = ' ';
        int index = 0;
        // Bypass any leading whitespace before first "(".
        while (isLeadingWhitespace) {
            if (index >= length) {
                Log.current.println("parsePerformative, invalid leading whitespace: " + ampString);
                return  "";
            }
            ch = ampString.charAt(index);
            if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                index++;
            else
                isLeadingWhitespace = false;
        }
        if (ch != '(') {
            Log.current.println("parsePerformative, missing '(': " + ampString);
            return  "";
        }
        // Bypass any whitespace between "(" and performative word.
        isLeadingWhitespace = true;
        index++;
        while (isLeadingWhitespace) {
            if (index >= length) {
                Log.current.println("parsePerformative, no performative: " + ampString);
                return  "";
            }
            ch = ampString.charAt(index);
            if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                index++;
            else
                isLeadingWhitespace = false;
        }
        // Parse the performative word.
        String workString = ampString.substring(index);
        int end = parseWordSExpression(workString, 0);
        return  workString.substring(0, end + 1);
    }

    /**
     * Returns the performative parameter value for the argument.
     *
     * @param keyword the AMP keyword for which the value is sought
     * @return the performative parameter value for the argument
     */
    public String parseAmpPerformativeParameter (String keyword) {
        boolean isLeadingWhitespace = true;
        char ch = ' ';
        int index = 0;
        String workString = ampString;
        int length = workString.length();
        // Bypass any leading whitespace before first "(".
        while (isLeadingWhitespace) {
            if (index >= length) {
                Log.current.println("parsePerformativeParameter, invalid leading whitespace: " + workString);
                return  "";
            }
            ch = workString.charAt(index);
            if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                index++;
            else
                isLeadingWhitespace = false;
        }
        if (ch != '(') {
            Log.current.println("parsePerformativeParameter, missing '(': " + workString);
            return  "";
        }
        // Bypass "(".
        index++;
        // Bypass performative.
        workString = workString.substring(index);
        length = workString.length();
        index = parseWordSExpression(workString, 0);
        String word = workString.substring(0, index + 1);
        // Loop until keyword is found.
        while (true) {
            // Bypass whitespace.
            isLeadingWhitespace = true;
            index++;
            while (isLeadingWhitespace) {
                if (index >= length)
                    // Not found.
                    return  "";
                ch = workString.charAt(index);
                if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                    index++;
                else
                    isLeadingWhitespace = false;
            }
            if (ch == ')')
                // End of AMP expression, not found.
                return  "";
            workString = workString.substring(index);
            // Parse the performative parameter.
            index = parseWordSExpression(workString, 0);
            word = workString.substring(0, index + 1);
            // Bypass whitespace.
            index++;
            workString = workString.substring(index);
            length = workString.length();
            index = 0;
            isLeadingWhitespace = true;
            while (isLeadingWhitespace) {
                if (index >= length)
                    return  "";
                ch = workString.charAt(index);
                if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                    index++;
                else
                    isLeadingWhitespace = false;
            }
            workString = workString.substring(index);
            length = workString.length();
            // Parse the performative parameter value.
            index = parseSExpression(workString);
            String value = workString.substring(0, index + 1);
            if (word.equals(keyword)) {
                if (value.charAt(0) == '\"') {
                    // Remove delimiting quotes from a string.
                    value = value.substring(1, value.length() - 1);
                }
                return  value;
            }
        }
    }

    /**
     * Returns an index indicating the end of the next symbolic expression substring.
     *
     * @param aString the input string containing a symbolic expression
     * @return an int index indicating the end of the next symbolic expression substring
     */
    public static int parseSExpression (String aString) {
        int length = aString.length();
        int parenLevel = 0;
        boolean isQuotedString = false;
        boolean isLeadingWhitespace = true;
        int charsParsed = 0;
        char ch = ' ';
        int index = 0;
        // Bypass any leading whitespace before first "(".
        while (isLeadingWhitespace) {
            if (index >= length) {
                Log.current.println("parseSExpression, invalid string: " + aString);
                return  0;
            }
            ch = aString.charAt(index);
            if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == '\f'))
                index++;
            else
                isLeadingWhitespace = false;
        }
        if (ch == '(')
            return  parseParenSExpression(aString, index);
        else
            return  parseWordSExpression(aString, index);
    }

    /**
     * Returns an int index indicating the end of the next parenthesized
     * symbolic expression substring.
     *
     * @param aString the input string containing a symbolic expression
     * @param startingIndex the starting point for the search for the next
     * parenthesized symbolic expression substring
     * @return an int index indicating the end of the next parenthesized
     * symbolic expression substring
     */
    private static int parseParenSExpression (String aString, int startingIndex) {
        int length = aString.length();
        int parenLevel = 1;
        boolean isQuotedString = false;
        int charsParsed = 0;
        char ch = ' ';
        int index = startingIndex;
        while (parenLevel != 0) {
            index++;
            if (index >= length) {
                Log.current.println("parseParenSExpression, invalid string: " + aString);
                return  0;
            }
            ch = aString.charAt(index);
            if (ch == '"')
                if (isQuotedString)
                    isQuotedString = false;
                else
                    isQuotedString = true;
            if (!isQuotedString) {
                if (ch == '(')
                    parenLevel++;
                if (ch == ')')
                    parenLevel--;
            }
        }
        return  index;
    }

    /***
     * Return an int index indicating the end of the next word or quoted string
     * symbolic expression substring.
     *
     * @param aString the input string containing a  word or quoted string
     * @param startingIndex the starting point for the search for the next
     * next word or quoted string
     * @return an int index indicating the end of the next word or quoted string
     * symbolic expression substring
     */
    private static int parseWordSExpression (String aString, int startingIndex) {
        int length = aString.length();
        boolean isQuotedString = false;
        int charsParsed = 0;
        char ch = ' ';
        int index = startingIndex;
        ch = aString.charAt(index);
        if (ch == '"')
            isQuotedString = true;
        while (true) {
            index++;
            if (index >= length)
                if (isQuotedString) {
                    Log.current.println("parseWordSExpression, invalid string: " + aString);
                    return  0;
                }
                else
                    return  index - 1;
            ch = aString.charAt(index);
            if (!isQuotedString)
                if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r') || (ch == ')') || (ch
                        == '\f'))
                    // End of word.
                    return  index - 1;
            if (ch == '"') {
                if (isQuotedString) {
                    return  index;
                }
                else {
                    isQuotedString = true;
                }
            }
        }
    }

    /**
     * Test the methods of this class.
      */
    public static void test () {
        Log.current.println("parseSExpression(\"aWord\")");
        Log.current.println(parseSExpression("aWord"));
        Log.current.println("parseSExpression(\"  aWord  \")");
        Log.current.println(parseSExpression("  aWord  "));
        Log.current.println("parseSExpression(\"(aList)\")");
        Log.current.println(parseSExpression("(aList)"));
        Log.current.println("parseSExpression(\"(a (nested list))\")");
        Log.current.println(parseSExpression("(a (nested list))"));
        Log.current.println("parseSExpression(\"\"aString\"\")");
        Log.current.println(parseSExpression("\"aString\""));
        Amp amp1 = new Amp("(ACHIEVE)");
        Log.current.println("parsePerformative " + amp1.ampString());
        Log.current.println(amp1.parsePerformative());
        Amp amp2 = new Amp("  (ADVERTISE)");
        Log.current.println("parsePerformative " + amp2.ampString());
        Log.current.println(amp2.parsePerformative());
        Amp amp3 = new Amp("( ACHIEVE )");
        Log.current.println("parsePerformative " + amp3.ampString());
        Log.current.println(amp3.parsePerformative());
        Amp amp4 = new Amp("(ASK-ALL " + ":SENDER sender-value " + ":RECEIVER receiver-value " + ":REPLY-WITH reply-with-value "
                + ":IN-REPLY-TO in-reply-to-value " + ":CONTENT \"content-value\" " + ":LANGUAGE language-value "
                + ":ONTOLOGY ontology-value " + ":FORCE force-value)");
        Log.current.println("amp4: " + amp4.ampString());
        Log.current.println("performative " + amp4.parsePerformative());
        Log.current.println("sender " + amp4.parseAmpPerformativeParameter(":SENDER"));
        Log.current.println("receiver " + amp4.parseAmpPerformativeParameter(":RECEIVER"));
        Log.current.println("reply-with " + amp4.parseAmpPerformativeParameter(":REPLY-WITH"));
        Log.current.println("in-reply-to " + amp4.parseAmpPerformativeParameter(":IN-REPLY-TO"));
        Log.current.println("content " + amp4.parseAmpPerformativeParameter(":CONTENT"));
        Log.current.println("language " + amp4.parseAmpPerformativeParameter(":LANGUAGE"));
        Log.current.println("ontology " + amp4.parseAmpPerformativeParameter(":ONTOLOGY"));
        Log.current.println("force " + amp4.parseAmpPerformativeParameter(":FORCE"));
        Amp amp5 = new Amp("(ASK-ALL " + ":SENDER (sender-value) " + ":RECEIVER (receiver-value) " +
                ":REPLY-WITH (reply-with-value) " + ":IN-REPLY-TO (in-reply-to-value) " + ":CONTENT (content-value) "
                + ":LANGUAGE (language-value) " + ":ONTOLOGY ((ontology-value)) " + ":FORCE (force-value))");
        Log.current.println("amp5: " + amp5.ampString());
        Log.current.println("performative " + amp5.parsePerformative());
        Log.current.println("sender " + amp5.parseAmpPerformativeParameter(":SENDER"));
        Log.current.println("receiver " + amp5.parseAmpPerformativeParameter(":RECEIVER"));
        Log.current.println("reply-with " + amp5.parseAmpPerformativeParameter(":REPLY-WITH"));
        Log.current.println("in-reply-to " + amp5.parseAmpPerformativeParameter(":IN-REPLY-TO"));
        Log.current.println("content " + amp5.parseAmpPerformativeParameter(":CONTENT"));
        Log.current.println("language " + amp5.parseAmpPerformativeParameter(":LANGUAGE"));
        Log.current.println("ontology " + amp5.parseAmpPerformativeParameter(":ONTOLOGY"));
        Log.current.println("force " + amp5.parseAmpPerformativeParameter(":FORCE"));
        amp5.generateAmpString();
        Log.current.println("Generated amp5: \n" + amp5.ampString());
    }
}



