package  org.opencyc.util;

import  java.util.Date;

/**
 * Implements A timer that can be set to a specified number of seconds or milliseconds
 * before it times out, and can be checked for the time elapsed since the start
 * or time remaining until the timeout.<p>
 *
 * @version $Id: Timer.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Eric E. Allen<br>
 * @author Bjorn Aldag<br>
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
public class Timer {
    public static final Date APOCALYPSE = new Date(Long.MAX_VALUE);
    private Date timeOut = APOCALYPSE;
    private Date start;
    private long timeAlloted;

    /**
     * Creates and starts a new timer that will run the specified number of
     * seconds before timing out.
     *
     * @param timeAlloted the number of seconds the timer will run before timing
     * out.
     */
    public Timer (int timeAlloted) {
        this.start = new Date();
        this.timeAlloted = timeAlloted * 1000L;
        this.timeOut = new Date(start.getTime() + (((long)timeAlloted)*1000L));
    }

    /**
     * Creates and starts a new timer that will run the specified number of
     * milliseconds before timing out.
     *
     * @param timeAlloted the number of milliseconds the timer will run before
     * timing out.
     */
    public Timer (long timeAlloted) {
        this.start = new Date();
        this.timeAlloted = timeAlloted;
        this.timeOut = new Date(start.getTime() + timeAlloted);
    }

    /**
     * Creates and starts a new timer that will run forever.
     */
    public Timer () {
        this.start = new Date();
        this.timeAlloted = 0;
    }
    
    public long getAllotedMSecs() {
      return timeAlloted;
    }

    /**
     * Restarts this timer with the same timeout it had previously.
     */
    public void start () {
        Date now = new Date();
        this.timeOut = new Date(2*timeOut.getTime() - now.getTime());
        start = now;
    }

    /**
     * Returns the number of seconds that this timer has been running.
     *
     * @return the number of seconds that this timer has been running.
     */
    public int getElapsedSeconds () {
        return  (int)(getElapsedMilliSeconds()/1000L);
    }

    /**
     * Returns the number of milliseconds that this timer has been running.
     *
     * @return the number of milliseconds that this timer has been running.
     */
    public long getElapsedMilliSeconds () {
        return  new Date().getTime() - start.getTime();
    }

    /**
     * Returns the number of seconds remaining till the timeout of this timer.
     *
     * @return the number of seconds remaining till the timeout of this timer.
     */
    public int getRemainingSeconds () {
        return  (int)(getRemainingMilliSeconds()/1000L);
    }

    /**
     * Returns the number of milliseconds remaining till the timeout of this timer.
     *
     * @return the number of milliseconds remaining till the timeout of this timer.
     */
    public long getRemainingMilliSeconds () {
        return  timeOut.getTime() - new Date().getTime();
    }

    /**
     * Returns <code>true</code> if this timer is timed out, <code>false</code> otherwise.
     *
     * @return <code>true</code> if this timer is timed out, <code>false</code> otherwise.
     */
    public boolean isTimedOut () {
        return  (new Date().after(timeOut));
    }

    /**
     * Throws a @see TimeOutException if this timer has been running longer than
     * the timeOut.
     *
     * @exception TimeOutException if this timer has run the specifier number of
     * seconds.
     */
    public void checkForTimeOut () throws TimeOutException {
        if (isTimedOut()) {
            throw  new TimeOutException("");
        }
    }
}



