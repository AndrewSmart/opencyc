/* $Id: CfaslInputStreamClosedException.java 126640 2008-12-04 13:39:36Z builder $
 *
 * Copyright (c) 2003 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.api;

//// Internal Imports

//// External Imports

/**
 * Class CfaslInputStreamClosedException indicates that the peer (usually the Cyc server)
 * closed the socket connection.
 *
 * @version $Id: CfaslInputStreamClosedException.java 126640 2008-12-04 13:39:36Z builder $
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
public class CfaslInputStreamClosedException extends RuntimeException {
  
  //// Constructors
  
  /** Creates a new instance of CfaslInputStreamClosedException. */
  public CfaslInputStreamClosedException() {
  }
  
  //// Public Area
  
    /**
     * Construct a CfaslInputStreamClosedException object with a specified message.
     * @param s a message describing the exception.
     */
    public CfaslInputStreamClosedException(String s) {
        super(s);
    }
    
    public CfaslInputStreamClosedException(String s, Throwable cause) {
      super(s, cause);
    }
    
    public CfaslInputStreamClosedException(Throwable cause) {
      super(cause);
    }
    
  //// Protected Area
  
  //// Private Area
  
  //// Internal Rep
  
  //// Main
  
}
