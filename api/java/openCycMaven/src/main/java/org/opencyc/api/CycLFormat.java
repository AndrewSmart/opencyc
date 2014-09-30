/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.opencyc.api;

import java.io.IOException;
import java.net.UnknownHostException;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.opencyc.cycobject.CycList;
import org.opencyc.cycobject.DefaultCycObject;

/**
 *
 * @author daves
 */
public class CycLFormat extends Format {

    private CycAccess cyc;
    private boolean newlines = true;
    private boolean showHashDollar = true;
    protected CycLFormat(CycAccess cyc) {
        this.cyc = cyc;
    }

    /**
     * Get a default NLFormat instance that will use the specified CycAccess.
     */
    public static CycLFormat getInstance(CycAccess cyc) {
        final CycLFormat cycF = new CycLFormat(cyc);
        return cycF;
    }

    public void setWrapLines(boolean newlines) {
        this.newlines = newlines;
    }
    
    public void setShowHashDollar(boolean showHashDollar) {
        this.showHashDollar = showHashDollar;
    }

    private CycAccess getCycAccess() {
        return cyc;
    }
    
    private String buildFormatCommand(Object obj) {
        return "(get-pretty-formatted-string " + DefaultCycObject.stringApiValue(obj) + " " 
                + DefaultCycObject.stringApiValue(showHashDollar) + " " 
                + DefaultCycObject.stringApiValue(newlines) + ")";
    }
    
    @Override
    public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
        final String command = buildFormatCommand(obj);
        try {
            toAppendTo.append(getCycAccess().converseString(command));
        } catch (Exception ex) {
            throw new RuntimeException("Exception formatting " + obj, ex);
        }
        return toAppendTo;
    }

    @Override
    public Object parseObject(String source, ParsePosition pos) {
        try {
            String command = "(multiple-value-list (read-from-string-ignoring-errors (cyclify-string " + DefaultCycObject.stringApiValue(source) + ")))";
            CycList ret = (CycList) getCycAccess().converseCycObject(command);
            Object value = ret.first();
            Object indexOrError = ret.second();
            if (indexOrError instanceof Integer) {
                pos.setIndex((Integer) indexOrError);
                return value;
            } else {
                pos.setErrorIndex(pos.getIndex());
                return null;
            }
        } catch (UnknownHostException ex) {
            Logger.getLogger(CycLFormat.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(CycLFormat.class.getName()).log(Level.SEVERE, null, ex);
        } catch (CycApiException ex) {
            Logger.getLogger(CycLFormat.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
}
